//! Differential tests of the cleaner against GCC, Clang and cpp.

use std::{
    collections::HashSet,
    io::Write as _,
    process::{Command, Stdio},
};

use arbitrary::{Arbitrary, Result, Unstructured};
use camino::Utf8Path;
use dt_tools_preprocessor::{PpTokenKind, PreprocessorConfig, lex, preprocess};
use logos::Logos;

/// Fixed seed, so the generated corpus is identical on every run.
const SEED: u64 = 0x5EED_2026_1018;

/// Default number of generated inputs. Extend the sequence with `PREPROCESSOR_DIFFERENTIAL_ITERS`.
const DEFAULT_ITERATIONS: usize = 50;

const CPP_ARGS: &[&str] = &[
    // Same as in Linux's scripts/Makefile.dtbs.
    "-E",        // Preprocess only.
    "-nostdinc", // No system include directories.
    "-undef",    // Drop predefined macros.
    "-D",
    "__DTS__", // Define __DTS__ macro.
    // Assembler-like input, so unknown `#` lines such as `#address-cells` pass through instead of
    // being rejected as directives.
    "-x",
    "assembler-with-cpp",
    // Added by us
    "-P", // No line markers in the output.
    "-",  // Read the input from stdin.
];

/// Inputs that never form a directive. A `#` only appears glued to a non-directive name.
#[derive(Arbitrary, Clone, Copy, strum::Display)]
enum CleanFragment {
    // Identifiers and `#`-names.
    #[strum(serialize = "foo")]
    Identifier,
    #[strum(serialize = "#address")]
    HashName,
    // Numbers.
    #[strum(serialize = "0x1f")]
    HexNumber,
    #[strum(serialize = "12abc")]
    NumberSuffix,
    #[strum(serialize = "1.5e+3")]
    Float,
    #[strum(serialize = "0xE+foo")]
    HexPlus,
    // Comment starters and line continuations.
    #[strum(serialize = "/")]
    Slash,
    #[strum(serialize = "*")]
    Star,
    #[strum(serialize = "\\")]
    Backslash,
    // `::` is C++ punctuation, so it is two tokens here.
    #[strum(serialize = "::")]
    DoubleColon,
    // Strings and characters.
    #[strum(serialize = "\"a b\"")]
    String,
    #[strum(serialize = "\"/*\"")]
    CommentString,
    #[strum(serialize = "\"a\\\\\"")]
    EscapedString,
    #[strum(serialize = "'a'")]
    Char,
    #[strum(serialize = "'\\''")]
    EscapedChar,
    #[strum(serialize = "'a")]
    UnterminatedChar,
    // Comments and line continuations.
    #[strum(serialize = "/* multi\nline */")]
    BlockComment,
    #[strum(serialize = "// line\n")]
    LineComment,
    #[strum(serialize = "// cont\\\ninued\n")]
    ContinuedComment,
    #[strum(serialize = "\\\n")]
    Splice,
    #[strum(serialize = "\\\\\n")]
    DoubleSplice,
    // Whitespace.
    #[strum(serialize = " ")]
    Space,
    #[strum(serialize = "\n")]
    Newline,
    #[strum(serialize = "\r")]
    CarriageReturn,
    #[strum(serialize = "\r\n")]
    CarriageReturnNewline,
    #[strum(serialize = "\t")]
    Tab,
    // Node names with unit addresses.
    #[strum(serialize = "uart@fe001000")]
    UnitAddress,
}

/// A generated cleaning case.
struct CleanCase {
    fragments: Vec<CleanFragment>,
}

impl<'a> Arbitrary<'a> for CleanCase {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let count = u.int_in_range(1..=MAX_FRAGMENTS)?;
        let fragments = (0..count)
            .map(|_| CleanFragment::arbitrary(u))
            .collect::<Result<Vec<_>>>()?;
        Ok(Self { fragments })
    }
}

impl std::fmt::Display for CleanCase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fragment in &self.fragments {
            write!(f, "{fragment}")?;
        }
        Ok(())
    }
}

/// Hand-picked inputs covering lexer edge cases.
const EDGE_CASES: &[&str] = &[
    // DTS shapes that are not preprocessor directives.
    "foo = <1>;\n#address-cells = <1>;\n#size-cells = <0>;\n",
    "uart@fe001000 serial@1\n",
    // A `#` after another token does not start a directive.
    "\\#escaped\n",
    // Block comments, single-line and multi-line, become one space.
    "foo/*c*/bar\n",
    "a/* x\ny */b\n",
    // Line comments become one space.
    "foo//x\nbar\n",
    // A line continuation joins tokens.
    "FOO\\\nBAR = <1>;\n",
    // A doubled backslash is not a continuation.
    "a\\\\\n\nb\n",
    // Whitespace before the newline does not prevent a continuation.
    "foo\\  \nbar\n",
    // Splicing happens before comments are recognized.
    "a/\\\n* c */b\n",
    "a/\\\n/ c\nb\n",
    // Splicing inside string and character literals.
    "\"a\\\nb\" \"a\\\\\nb\" 'a\\\n'\n",
    // Preprocessing numbers.
    "12abc 1.5e+3 0xE+foo 1e+ .5 0 1..E+3.foo\n",
    // Punctuators are matched maximally, and `::` is two tokens.
    "<<= ... %:%: ## :: -> <= >= == != && ||\n",
    // Character constants, including empty ones and comment starters.
    "'a' 'ab' 'a b' '\\'' '' '/*' '//'\n",
    // String literals, including comment starters.
    "\"a b\" \"a\\\"b\" \"/*\" \"//\"\n",
    // `.`, `$` and `-` end an identifier.
    "foo.bar foo$bar foo-bar\n",
    // Other characters group into runs between identifiers.
    "a$b a$@\\$b\n",
    // CR and CRLF normalize to `\n`.
    "a\r\nb\rc\nd\n",
    // A UTF-8 BOM is stripped.
    "\u{feff}a\n",
    // Unterminated literals and comments.
    "\"abc\nx\n",
    "'a\nb\n",
    "a /* x\n",
];

/// A macro name from a fixed set.
#[derive(Arbitrary, Clone, Copy, PartialEq, Eq, Hash, strum::Display)]
enum MacroName {
    A,
    B,
    C,
    D,
}

/// A macro parameter name from a fixed set.
///
/// Separate from [`MacroName`] so generated parameters never collide with macros.
#[derive(Arbitrary, Clone, Copy, PartialEq, Eq, Hash, strum::Display)]
enum ParamName {
    X,
    Y,
    Z,
}

/// A fragment of the generated macro corpus.
#[derive(Arbitrary)]
enum MacroFragment {
    Undef(MacroName),
    Define {
        name: MacroName,
        /// `None` for an object-like definition, `Some` for a function-like
        /// one. Inner `bool` represents whether the definition is variadic.
        #[arbitrary(with = params)]
        params: Option<(Vec<ParamName>, bool)>,
        #[arbitrary(with = body_tokens)]
        body: Vec<BodyToken>,
    },
    Use(Use),
}

/// Generates a bounded parameter list.
fn params(u: &mut Unstructured<'_>) -> Result<Option<(Vec<ParamName>, bool)>> {
    if !u.arbitrary()? {
        return Ok(None);
    }

    let count = u.int_in_range(0..=MAX_PARAMS)?;
    let params = (0..count)
        .map(|_| ParamName::arbitrary(u))
        .collect::<Result<Vec<_>>>()?;
    Ok(Some((params, u.arbitrary()?)))
}

/// Generates a bounded replacement list.
fn body_tokens(u: &mut Unstructured<'_>) -> Result<Vec<BodyToken>> {
    let count = u.int_in_range(0..=MAX_BODY_TOKENS)?;
    (0..count).map(|_| BodyToken::arbitrary(u)).collect()
}

impl std::fmt::Display for MacroFragment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Undef(name) => writeln!(f, "#undef {name}"),
            Self::Define { name, params, body } => {
                // Remove a previous definition first, so a redefinition does not warn.
                writeln!(f, "#undef {name}")?;
                write!(f, "#define {name}")?;

                let function_like = params.is_some();
                let (params, is_variadic) = match params {
                    Some((params, is_variadic)) => (dedup(params), *is_variadic),
                    None => (Vec::new(), false),
                };

                if function_like {
                    write!(f, "(")?;
                    for (index, param) in params.iter().enumerate() {
                        if index > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{param}")?;
                    }
                    if is_variadic {
                        if !params.is_empty() {
                            write!(f, ", ")?;
                        }
                        write!(f, "...")?;
                    }
                    write!(f, ")")?;
                }

                if !body.is_empty() {
                    write!(
                        f,
                        " {}",
                        Body {
                            tokens: body,
                            params: &params,
                            is_variadic,
                        }
                    )?;
                }
                writeln!(f)
            }
            Self::Use(item) => writeln!(f, "{item}"),
        }
    }
}

/// A replacement list with its definition context.
struct Body<'a> {
    tokens: &'a [BodyToken],
    params: &'a [ParamName],
    is_variadic: bool,
}

impl std::fmt::Display for Body<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for token in self.tokens {
            // `__VA_ARGS__` outside a variadic macro is an error in the oracle.
            if matches!(token, BodyToken::VaArgs { .. }) && !self.is_variadic {
                continue;
            }
            if !first {
                f.write_str(" ")?;
            }
            match token {
                // `#` requires a parameter. The oracle treats the invalid form as ordinary
                // tokens, so do not generate it.
                BodyToken::Stringify(param) if !self.params.contains(param) => {
                    if self.is_variadic {
                        f.write_str("#__VA_ARGS__")?;
                    } else {
                        write!(f, "{param}")?;
                    }
                }
                _ => write!(f, "{token}")?,
            }
            first = false;
        }
        Ok(())
    }
}

/// Returns `params` without duplicates, preserving order.
fn dedup(params: &[ParamName]) -> Vec<ParamName> {
    let mut seen = HashSet::new();
    params
        .iter()
        .copied()
        .filter(|param| seen.insert(*param))
        .collect()
}

/// A token in a macro replacement list.
#[derive(Arbitrary, strum::Display)]
enum BodyToken {
    #[strum(transparent)]
    Value(Value),
    #[strum(transparent)]
    Param(ParamName),
    /// `#param`.
    #[strum(to_string = "#{0}")]
    Stringify(ParamName),
    /// `left ## right`.
    #[strum(to_string = "{0}##{1}")]
    Concat(PasteOperand, PasteOperand),
    /// `__VA_ARGS__`, optionally with GNU comma elision.
    #[strum(transparent)]
    VaArgs(VaArgs),
}

#[derive(Arbitrary)]
struct VaArgs {
    gnu_comma: bool,
}
impl std::fmt::Display for VaArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.gnu_comma {
            f.write_str(", ## ")?;
        }
        f.write_str("__VA_ARGS__")
    }
}

/// An operand of `##`, restricted to tokens that paste into one pp-token.
#[derive(Arbitrary, strum::Display)]
enum PasteOperand {
    #[strum(transparent)]
    Param(ParamName),
    #[strum(transparent)]
    MacroName(MacroName),
    #[strum(serialize = "0")]
    Number,
}

/// A value usable as an argument or as a replacement token.
#[derive(strum::Display)]
enum Value {
    #[strum(transparent)]
    Invoke(Invoke),
    #[strum(serialize = "1")]
    Number1,
    #[strum(serialize = "\"s\"")]
    String,
    #[strum(to_string = "{0} + {1}")]
    Plus(Box<Value>, Box<Value>),
}

impl<'a> Arbitrary<'a> for Value {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        value(u, MAX_VALUE_DEPTH)
    }
}

/// Generates a value with at most `depth` levels of nesting.
fn value(u: &mut Unstructured<'_>, depth: u8) -> Result<Value> {
    if depth == 0 {
        return Ok(if u.arbitrary()? {
            Value::Number1
        } else {
            Value::String
        });
    }

    match u.int_in_range(0..=3)? {
        0 => Ok(Value::Invoke(invoke(u, depth)?)),
        1 => Ok(Value::Number1),
        2 => Ok(Value::String),
        _ => Ok(Value::Plus(
            Box::new(value(u, depth - 1)?),
            Box::new(value(u, depth - 1)?),
        )),
    }
}

/// A macro invocation.
struct Invoke {
    name: MacroName,
    /// `None` renders the name alone.
    args: Option<Args>,
}

impl std::fmt::Display for Invoke {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(args) = &self.args {
            if args.space_before_lparen {
                f.write_str(" ")?;
            }
            write!(f, "{args}")?;
        }
        Ok(())
    }
}

impl<'a> Arbitrary<'a> for Invoke {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        invoke(u, MAX_VALUE_DEPTH)
    }
}

/// Generates an invocation with at most `depth` levels of nesting.
fn invoke(u: &mut Unstructured<'_>, depth: u8) -> Result<Invoke> {
    let name = MacroName::arbitrary(u)?;
    let args = if depth > 0 && u.arbitrary()? {
        Some(args(u, depth)?)
    } else {
        None
    };
    Ok(Invoke { name, args })
}

/// A parenthesized argument list.
struct Args {
    /// Adds an extra pair of parentheses around the joined arguments.
    wrap_in_parentheses: bool,
    /// Puts a space between the macro name and `(`.
    space_before_lparen: bool,
    values: Vec<Value>,
}

impl std::fmt::Display for Args {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(")?;
        if self.wrap_in_parentheses {
            f.write_str("(")?;
        }
        for (index, value) in self.values.iter().enumerate() {
            if index > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{value}")?;
        }
        if self.wrap_in_parentheses {
            f.write_str(")")?;
        }
        f.write_str(")")
    }
}

impl<'a> Arbitrary<'a> for Args {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        args(u, MAX_VALUE_DEPTH)
    }
}

/// Generates an argument list whose values nest at most `depth` levels.
fn args(u: &mut Unstructured<'_>, depth: u8) -> Result<Args> {
    let count = u.int_in_range(0..=MAX_ARGS)?;
    let values = (0..count)
        .map(|_| value(u, depth.saturating_sub(1)))
        .collect::<Result<Vec<_>>>()?;
    Ok(Args {
        wrap_in_parentheses: u.arbitrary()?,
        space_before_lparen: u.arbitrary()?,
        values,
    })
}

/// A top-level use of a macro or ordinary source text.
#[derive(Arbitrary, strum::Display)]
enum Use {
    #[strum(transparent)]
    Invoke(Invoke),
    #[strum(to_string = "{0} {1}")]
    Pair(MacroName, MacroName),
    #[strum(to_string = "{name} = {value};")]
    Assign { name: MacroName, value: Value },
    #[strum(to_string = "{0}/*c*/{1}")]
    Commented(MacroName, MacroName),
    #[strum(serialize = "")]
    Blank,
}

/// A generated macro case.
struct MacroCase {
    fragments: Vec<MacroFragment>,
}

impl std::fmt::Display for MacroCase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fragment in &self.fragments {
            write!(f, "{fragment}")?;
        }
        Ok(())
    }
}

impl<'a> Arbitrary<'a> for MacroCase {
    fn arbitrary(u: &mut Unstructured<'a>) -> Result<Self> {
        let count = u.int_in_range(1..=MAX_FRAGMENTS)?;
        let mut fragments = Vec::with_capacity(count as usize);
        for _ in 0..count {
            // A fragment gets its own budget, so one recursive value cannot consume the rest.
            let Ok(bytes) = u.arbitrary::<[u8; FRAGMENT_BYTES]>() else {
                break;
            };
            let mut fragment_input = Unstructured::new(&bytes);
            if let Ok(fragment) = MacroFragment::arbitrary(&mut fragment_input) {
                fragments.push(fragment);
            }
        }
        Ok(Self { fragments })
    }
}

/// Hand-picked inputs covering macro edge cases.
const MACRO_EDGE_CASES: &[&str] = &[
    "#define A 1\nA\n",
    "#define A B\n#define B 2\nA\n",
    "#define A A\nA\n",
    "#define A B\n#define B A\nA\n",
    "#define A\nA x\n",
    "#define A 1\n#undef A\nA\n",
    "#define A foo bar\nA\n",
    "#define A 1\nA A\n",
    "#define A 1 A\nA\n",
    "#define PLUS +\nPLUS PLUS\n",
    "#define SLASH /\nSLASH*\n",
    "#define D .\nD D D\n",
    "#define cells 4\n/ { #size-cells = <1>; };\n",
    "#define A 1 // c\nA\n",
    "#define A 12\\\n3\nA\n",
    "#define A &gpio1\nx = A;\n",
    "  #define A 1\n  A\n",
    // Function-like macros.
    "#define F(x) x\nF(1)\n",
    "#define F(a, b) a b\nF(1, 2)\n",
    "#define F() 1\nF()\n",
    "#define F(x) x\nF (1)\n",
    "#define F(x) x\nF(F(1))\n",
    "#define F(x) [x]\nF()\n",
    "#define F(x) [x]\nF((1, 2))\n",
    "#define F(x) F(x)\nF(1)\n",
    "#define F G\n#define G(x) x\nF(1)\n",
    "#define P(x) &x\nP(gpio1)\n",
    // Variadic macros.
    "#define F(...) __VA_ARGS__\nF(1, 2)\n",
    "#define F(a, ...) a __VA_ARGS__\nF(1, 2, 3)\n",
    "#define F(a, ...) a __VA_ARGS__\nF(1)\n",
    "#define F(a, ...) a __VA_ARGS__\nF()\n",
    "#define F(...) __VA_ARGS__\nF()\n",
    "#define A 1\n#define F(...) __VA_ARGS__\nF(A, 2)\n",
    // Stringification.
    "#define S(x) #x\nS(abc)\n",
    "#define S(x) #x\nS(a b)\n",
    "#define S(x) #x\nS( a   b )\n",
    "#define S(x) #x\nS(\"a b\")\n",
    "#define S(x) #x\nS()\n",
    "#define A 1\n#define S(x) #x\nS(A)\n",
    "#define HASH #\nHASH\n",
    "#define S #x\nS\n",
    // Token pasting.
    "#define CAT(a, b) a ## b\nCAT(foo, bar)\n",
    "#define CAT(a, b) a ## b\nCAT(+, +)\n",
    "#define A 1\n#define B 2\n#define CAT(a, b) a ## b\nCAT(A, B)\n",
    "#define CAT(a, b) a ## b\nCAT(, x)\n",
    "#define CAT(a, b) a ## b\nCAT(x,)\n",
    "#define CAT(a, b) a ## b\nCAT(,)\n",
    "#define A 0 ## 0\nA\n",
    // GNU comma elision.
    "#define F(a, ...) a, ## __VA_ARGS__\nF(1)\n",
    "#define F(a, ...) a, ## __VA_ARGS__\nF(1, 2)\n",
    "#define F(a, ...) a, ## __VA_ARGS__\nF(1,)\n",
    // Malformed directives and invocations.
    "#define\n",
    "#undef\n",
    "#define 1 A\n",
    "#define F(a, b) a b\nF(1)\n",
    "#define F(x) x\nF(1\n",
];

/// A preprocessor to compare against.
struct Cpp {
    program: String,
}

impl Cpp {
    /// Returns every available preprocessor. If `CPP` is set, only that program is used.
    fn available() -> Vec<Self> {
        let candidates: Vec<String> = match std::env::var("CPP") {
            Ok(program) => vec![program],
            Err(_) => ["clang", "gcc", "cpp"].map(str::to_owned).to_vec(),
        };

        let cpps: Vec<Self> = candidates
            .into_iter()
            .filter(|program| responds_to_version(program))
            .map(|program| Self { program })
            .collect();

        if cpps.is_empty() {
            eprintln!("skipping differential tests, no cpp, gcc or clang found");
        }

        cpps
    }

    /// Runs the preprocessor on `input`, returning its stdout or its stderr if it failed.
    fn run(&self, input: &str) -> Result<String, String> {
        let mut child = Command::new(&self.program)
            .args(CPP_ARGS)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|error| error.to_string())?;

        child
            .stdin
            .take()
            .expect("stdin is piped")
            .write_all(input.as_bytes())
            .map_err(|error| error.to_string())?;

        let output = child
            .wait_with_output()
            .map_err(|error| error.to_string())?;
        if output.status.success() {
            Ok(String::from_utf8_lossy(&output.stdout).into_owned())
        } else {
            Err(String::from_utf8_lossy(&output.stderr).into_owned())
        }
    }
}

/// Returns true if `program` runs.
fn responds_to_version(program: &str) -> bool {
    Command::new(program)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

/// Tokenizes without cleaning. cpp's output is already free of comments and line continuations,
/// so it must not be cleaned again, and our cleaned text is compared to it as-is.
fn raw_tokens(text: &str) -> Vec<(PpTokenKind, String)> {
    let mut tokens = Vec::new();
    let mut lexer = PpTokenKind::lexer(text);
    while let Some(result) = lexer.next() {
        let kind = result.expect("text must lex without errors");
        tokens.push((kind, lexer.slice().to_owned()));
    }
    tokens
}

/// The preprocessors' verdict on one input.
enum Consensus {
    /// Every preprocessor rejected the input.
    Rejected,
    /// Every preprocessor produced the same tokens.
    Tokens {
        expected: Vec<(PpTokenKind, String)>,
        output: String,
    },
    /// The preprocessors disagree, so there is no oracle.
    Disagreement,
}

/// Collects the preprocessors' consensus for `input`.
fn consensus(cpps: &[Cpp], input: &str, index: usize, label: &str) -> Consensus {
    if cpps.is_empty() {
        return Consensus::Disagreement;
    }

    // Clang appends a NUL when the input ends with a line continuation. The C standard calls
    // that undefined behavior, so ignore it.
    let outputs: Vec<String> = cpps
        .iter()
        .filter_map(|cpp| cpp.run(input).ok())
        .map(|output| output.replace('\0', ""))
        .collect();

    if outputs.len() != cpps.len() {
        if outputs.is_empty() {
            return Consensus::Rejected;
        }
        // A mix is a disagreement, so there is no oracle.
        eprintln!("skipping {label} input {index}: preprocessors disagree");
        return Consensus::Disagreement;
    }

    let output = outputs[0].clone();
    let expected = raw_tokens(&output);
    if outputs
        .iter()
        .skip(1)
        .any(|output| raw_tokens(output) != expected)
    {
        eprintln!("skipping {label} input {index}: preprocessors disagree");
        return Consensus::Disagreement;
    }

    Consensus::Tokens { expected, output }
}

/// Asserts that our cleaned text matches the preprocessors when they agree.
fn check_consensus(cpps: &[Cpp], input: &str, index: usize) {
    let cleaned = lex(input);

    match consensus(cpps, input, index, "cleaned") {
        Consensus::Rejected => {
            // If every preprocessor rejected the input, we must reject it too.
            assert!(
                !cleaned.errors.is_empty(),
                "every preprocessor rejected the input but we did not\n--- input ---\n{input}",
            );
        }
        Consensus::Tokens { expected, output } => {
            assert_eq!(
                raw_tokens(&cleaned.cleaned_text),
                expected,
                "token sequence mismatch at index {index}\n--- input ---\n{input}\n--- cleaned ---\n{}\n--- cpp output ---\n{output}",
                cleaned.cleaned_text,
            );
        }
        Consensus::Disagreement => {}
    }
}

/// Returns our preprocessing result for `input`.
fn preprocess_input(input: &str) -> dt_tools_preprocessor::PreprocessedRoot {
    preprocess(
        Utf8Path::new("test.dts"),
        &PreprocessorConfig::default(),
        |_| Some(input.to_owned()),
    )
}

/// Asserts that our expanded text matches the preprocessors when they agree.
fn check_expanded_consensus(cpps: &[Cpp], input: &str, index: usize) {
    let result = preprocess_input(input);

    match consensus(cpps, input, index, "expanded") {
        Consensus::Rejected => {
            assert!(
                !result.diagnostics.is_empty(),
                "every preprocessor rejected the input but we did not\n--- input ---\n{input}",
            );
        }
        Consensus::Tokens { expected, output } => {
            assert!(
                result.diagnostics.is_empty(),
                "unexpected diagnostics at index {index}, other preprocessors succeeded and agreed\n--- input ---\n{input}\n--- diagnostics ---\n{:?}\n--- cpp output ---\n{output}",
                result.diagnostics
            );
            assert_eq!(
                raw_tokens(&result.text),
                expected,
                "expanded token sequence mismatch at index {index}\n--- input ---\n{input}\n--- ours ---\n{}\n--- cpp output ---\n{output}",
                result.text,
            );
        }
        Consensus::Disagreement => {}
    }
}

/// A deterministic xorshift64* PRNG.
pub struct Xorshift64Star {
    state: u64,
}

impl Xorshift64Star {
    /// Creates a new generator.
    ///
    /// # Panics
    ///
    /// Panics if the seed is 0.
    #[must_use]
    pub fn new(seed: u64) -> Self {
        assert!(seed != 0, "Xorshift64* seed cannot be zero!");
        Self { state: seed }
    }

    /// Generates the next random 64-bit integer.
    pub fn next_u64(&mut self) -> u64 {
        let mut x = self.state;
        x ^= x >> 12; // Triple shifts (a, b, c constants)
        x ^= x << 25;
        x ^= x >> 27;
        self.state = x;

        // Multiplied by Marsaglia's M_32 constant for non-linear scrambling
        x.wrapping_mul(0x2545_F491_4F6C_DD1D)
    }
}

/// Deterministic bytes fed to `Arbitrary` for each generated case.
const GENERATED_BYTES: usize = 2048;

/// Deterministic bytes reserved for one generated macro fragment.
const FRAGMENT_BYTES: usize = 128;

/// The maximum number of fragments in a generated case.
const MAX_FRAGMENTS: u8 = 8;

/// The maximum number of parameters in a generated definition.
const MAX_PARAMS: u8 = 3;

/// The maximum number of tokens in a generated replacement list.
const MAX_BODY_TOKENS: u8 = 6;

/// The maximum number of arguments in a generated invocation.
const MAX_ARGS: u8 = 4;

/// The maximum nesting depth of a generated value.
const MAX_VALUE_DEPTH: u8 = 3;

/// A deterministic source of `Arbitrary` cases.
struct Corpus {
    prng: Xorshift64Star,
}

impl Corpus {
    fn new() -> Self {
        Self {
            prng: Xorshift64Star::new(SEED),
        }
    }

    /// Generates one case from a fixed number of deterministic bytes.
    fn next<T>(&mut self) -> Option<T>
    where
        T: for<'a> Arbitrary<'a>,
    {
        let mut bytes = Vec::with_capacity(GENERATED_BYTES);
        while bytes.len() < GENERATED_BYTES {
            bytes.extend_from_slice(&self.prng.next_u64().to_le_bytes());
        }

        T::arbitrary(&mut Unstructured::new(&bytes)).ok()
    }
}

/// Returns the number of generated inputs to run.
fn iterations() -> usize {
    std::env::var("PREPROCESSOR_DIFFERENTIAL_ITERS")
        .ok()
        .and_then(|value| value.parse().ok())
        .unwrap_or(DEFAULT_ITERATIONS)
}

/// Checks a deterministic generated corpus against the available preprocessors.
#[test]
fn generated() {
    let cpps = Cpp::available();
    let mut corpus = Corpus::new();
    for index in 0..iterations() {
        let Some(case) = corpus.next::<CleanCase>() else {
            continue;
        };
        check_consensus(&cpps, &case.to_string(), index);
    }
}

/// Checks every hand-picked edge case against the available preprocessors.
#[test]
fn edge_cases() {
    let cpps = Cpp::available();
    for (index, input) in EDGE_CASES.iter().enumerate() {
        check_consensus(&cpps, input, index);
    }
}

/// Checks a deterministic generated macro corpus against the available preprocessors.
#[test]
fn generated_macros() {
    let cpps = Cpp::available();
    let mut corpus = Corpus::new();
    for index in 0..iterations() {
        let Some(case) = corpus.next::<MacroCase>() else {
            continue;
        };
        check_expanded_consensus(&cpps, &case.to_string(), index);
    }
}

#[test]
fn generated_cases_have_several_fragments() {
    // Guards the generator against one fragment consuming the whole case budget.
    let mut corpus = Corpus::new();
    let mut total = 0;
    for _ in 0..20 {
        let case = corpus.next::<MacroCase>().expect("a generated case");
        assert!(!case.fragments.is_empty(), "generated an empty case");
        total += case.fragments.len();
    }
    assert!(
        total >= 40,
        "generated cases are too small: {total} fragments"
    );
}

/// Checks every hand-picked macro edge case against the available preprocessors.
#[test]
fn macro_edge_cases() {
    let cpps = Cpp::available();
    for (index, input) in MACRO_EDGE_CASES.iter().enumerate() {
        check_expanded_consensus(&cpps, input, index);
    }
}
