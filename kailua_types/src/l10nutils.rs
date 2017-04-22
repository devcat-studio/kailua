use std::fmt;
use std::cell::RefCell;
use kailua_diag::message::{Locale, Localize};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ordinal(pub usize);

impl Localize for Ordinal {
    fn fmt_localized(&self, f: &mut fmt::Formatter, locale: Locale) -> fmt::Result {
        match &locale[..] {
            "ko" => {
                let w = match self.0 {
                    0 => "첫번째",
                    1 => "두번째",
                    2 => "세번째",
                    3 => "네번째",
                    4 => "다섯번째",
                    5 => "여섯번째",
                    6 => "일곱번째",
                    7 => "여덟번째",
                    8 => "아홉번째",
                    i => return write!(f, "{}번째", i + 1),
                };
                write!(f, "{}", w)
            },

            _ => {
                let (wc, wl) = match self.0 {
                    0 => ("First",   "first"),
                    1 => ("Second",  "second"),
                    2 => ("Third",   "third"),
                    3 => ("Fourth",  "fourth"),
                    4 => ("Fifth",   "fifth"),
                    5 => ("Sixth",   "sixth"),
                    6 => ("Seventh", "seventh"),
                    7 => ("Eighth",  "eighth"),
                    8 => ("Ninth",   "ninth"),
                    i if i % 10 == 0 && i % 100 != 10 => return write!(f, "{}-st", i + 1),
                    i if i % 10 == 1 && i % 100 != 10 => return write!(f, "{}-nd", i + 1),
                    i if i % 10 == 2 && i % 100 != 10 => return write!(f, "{}-rd", i + 1),
                    i => return write!(f, "{}-th", i + 1),
                };
                if f.sign_plus() {
                    write!(f, "{}", wc)
                } else {
                    write!(f, "{}", wl)
                }
            },
        }
    }
}

pub struct QuotedList<'a, I: Iterator<Item=&'a Localize>> {
    iter: RefCell<Option<I>>,
    locale: Locale,
}

impl<'a, I: Iterator<Item=&'a Localize>> QuotedList<'a, I> {
    pub fn new(iter: I, locale: Locale) -> QuotedList<'a, I> {
        QuotedList { iter: RefCell::new(Some(iter)), locale: locale }
    }
}

impl<'a, I: Iterator<Item=&'a Localize>> fmt::Display for QuotedList<'a, I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut iter = self.iter.borrow_mut().take().expect(
            "QuotedList can be formatted only once"
        );

        let mut cur = if let Some(cur) = iter.next() {
            cur
        } else {
            return Ok(());
        };

        write!(f, "`")?;

        // delayed iteration to handle the last "and"
        let mut first = true;
        while let Some(next) = iter.next() {
            if first {
                first = false;
            } else {
                write!(f, "`, `")?;
            }

            // keep the formatter flags down to elements
            cur.fmt_localized(f, self.locale)?;
            cur = next;
        }

        if !first {
            match &self.locale[..] {
                "ko" => write!(f, "` 및 `")?,
                _ => write!(f, "` and `")?,
            }
        }
        cur.fmt_localized(f, self.locale)?;
        write!(f, "`")
    }
}

