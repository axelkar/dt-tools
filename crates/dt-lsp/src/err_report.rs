use std::fmt::{self, Write as _};

/// Indents the formatted item inside.
pub struct IndentAdapterFmt<T> {
    inner: T,
    indent_first_line: bool,
    indent: &'static str,
}

impl<T> IndentAdapterFmt<T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner,
            indent_first_line: true,
            indent: "    ",
        }
    }
    pub fn indent_first_line(mut self, indent_first_line: bool) -> Self {
        self.indent_first_line = indent_first_line;
        self
    }
    pub fn indent(mut self, indent: &'static str) -> Self {
        self.indent = indent;
        self
    }
}

impl<T: fmt::Display> fmt::Display for IndentAdapterFmt<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(
                IndentAdapterFmtWrite {
                    f,
                    is_after_newline: self.indent_first_line,
                    indent: self.indent,
                },
                "{:#}",
                self.inner
            )
        } else {
            write!(
                IndentAdapterFmtWrite {
                    f,
                    is_after_newline: self.indent_first_line,
                    indent: self.indent,
                },
                "{:}",
                self.inner
            )
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for IndentAdapterFmt<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(
                IndentAdapterFmtWrite {
                    f,
                    is_after_newline: self.indent_first_line,
                    indent: self.indent,
                },
                "{:#?}",
                self.inner
            )
        } else {
            write!(
                IndentAdapterFmtWrite {
                    f,
                    is_after_newline: self.indent_first_line,
                    indent: self.indent,
                },
                "{:?}",
                self.inner
            )
        }
    }
}

struct IndentAdapterFmtWrite<'a, 'b> {
    f: &'a mut fmt::Formatter<'b>,
    is_after_newline: bool,
    indent: &'static str,
}

impl fmt::Write for IndentAdapterFmtWrite<'_, '_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for s in s.split_inclusive('\n') {
            if self.is_after_newline {
                self.f.write_str(self.indent)?;
            }

            self.is_after_newline = s.ends_with('\n');
            self.f.write_str(s)?;
        }

        Ok(())
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        if self.is_after_newline {
            self.f.write_str(self.indent)?;
        }
        self.is_after_newline = c == '\n';
        self.f.write_char(c)
    }
}

/// An error reporter that prints an error and its sources.
///
/// Basically [`std::error::Report`], but for use now.
pub struct Report<E>(E);

impl<E> Report<E>
where
    Report<E>: From<E>,
{
    pub fn new(error: E) -> Self {
        Self::from(error)
    }
}

impl<E: std::error::Error> fmt::Display for Report<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut curr_err = self.0.source();
        let sources = std::iter::from_fn(move || {
            let prev_err = curr_err;
            curr_err = curr_err?.source();
            prev_err
        })
        .collect::<Vec<_>>();

        write!(f, "{}", self.0)?;
        if !sources.is_empty() {
            write!(f, "\n\nCaused by:")?;
            if let [source] = sources[..] {
                // Only a single source
                write!(f, "\n{}", IndentAdapterFmt::new(source).indent("      "))?;
            } else {
                for (i, source) in sources.iter().enumerate() {
                    write!(
                        f,
                        "\n{i:>4}: {}",
                        IndentAdapterFmt::new(source)
                            .indent_first_line(false)
                            .indent("      ")
                    )?;
                }
            }
        }
        Ok(())
    }
}

impl<E: std::error::Error> fmt::Debug for Report<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<E: std::error::Error> From<E> for Report<E> {
    fn from(error: E) -> Self {
        Self(error)
    }
}
