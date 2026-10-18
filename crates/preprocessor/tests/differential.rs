//! Differential tests of the cleaner against GCC, Clang and cpp.

use std::{
    io::Write as _,
    process::{Command, Stdio},
};

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
const FRAGMENTS: &[&str] = &[
    // Identifiers and `#`-names.
    "foo",
    "#address",
    // Numbers.
    "0x1f",
    "12abc",
    "1.5e+3",
    "0xE+foo",
    // Comment starters and line continuations.
    "/",
    "*",
    "\\",
    // `::` is C++ punctuation, so it is two tokens here.
    "::",
    // Strings and characters.
    "\"a b\"",
    "\"/*\"",
    "\"a\\\\\"",
    "'a'",
    "'\\''",
    "'a",
    // Comments and line continuations.
    "/* multi\nline */",
    "// line\n",
    "// cont\\\ninued\n",
    "\\\n",
    "\\\\\n",
    // Whitespace.
    " ",
    "\n",
    "\r",
    "\r\n",
    "\t",
    // Node names with unit addresses.
    "uart@fe001000",
];

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

/// Line-sized fragments for the generated macro corpus.
const MACRO_FRAGMENTS: &[&str] = &[
    "#define A 1\n",
    "#define B A\n",
    "#define C 2\n",
    "#undef A\n",
    "#undef B\n",
    "\n",
    "A\n",
    "B\n",
    "C\n",
    "A B\n",
    "A + B\n",
    "A;A\n",
    "x = A;\n",
    "A/*c*/B\n",
    "#define F(x) x\n",
    "#define G(a, b) a b\n",
    "#define V(...) __VA_ARGS__\n",
    "#define W(a, ...) a __VA_ARGS__\n",
    "F(1)\n",
    "F(A)\n",
    "F (1)\n",
    "F(F(1))\n",
    "F((1, 2))\n",
    "G(1, 2)\n",
    "V(1, 2)\n",
    "V()\n",
    "W(1, 2, 3)\n",
];

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

    /// Generated the next random integer less than `n`.
    #[expect(
        clippy::missing_panics_doc,
        reason = "value is below n, which is below usize"
    )]
    pub fn below(&mut self, n: usize) -> usize {
        let value = self.next_u64() % n as u64;
        usize::try_from(value).unwrap()
    }
}

/// Deterministic input generator from a fragment list.
struct Generator {
    prng: Xorshift64Star,
    fragments: &'static [&'static str],
}

impl Generator {
    fn new(fragments: &'static [&'static str]) -> Self {
        Self {
            prng: Xorshift64Star::new(SEED),
            fragments,
        }
    }

    fn input(&mut self) -> String {
        let count = 1 + self.prng.below(40);
        let fragment_count = self.fragments.len();
        let mut input = String::new();
        for _ in 0..count {
            let index = self.prng.below(fragment_count);
            input.push_str(self.fragments[index]);
        }
        input
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
    let mut generator = Generator::new(FRAGMENTS);
    for index in 0..iterations() {
        check_consensus(&cpps, &generator.input(), index);
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
    let mut generator = Generator::new(MACRO_FRAGMENTS);
    for index in 0..iterations() {
        check_expanded_consensus(&cpps, &generator.input(), index);
    }
}

/// Checks every hand-picked macro edge case against the available preprocessors.
#[test]
fn macro_edge_cases() {
    let cpps = Cpp::available();
    for (index, input) in MACRO_EDGE_CASES.iter().enumerate() {
        check_expanded_consensus(&cpps, input, index);
    }
}
