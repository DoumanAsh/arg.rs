///Simple split of string into arguments
pub struct Split<'a> {
    string: &'a str,
}

impl<'a> Split<'a> {
    ///Creates new instance
    pub const fn from_str(string: &'a str) -> Self {
        Self {
            string
        }
    }

    #[inline(always)]
    ///Retrieves next argument
    pub fn next_arg(&mut self) -> Option<&str> {
        self.next()
    }
}

impl<'a> Iterator for Split<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        self.string = self.string.trim_start();

        if self.string.is_empty() {
            return None;
        }

        let (end_char, from) = match self.string.get(0..=0) {
            Some("'") => ('\'', 1),
            Some("\"") => ('"', 1),
            _ => (' ', 0),
        };

        match self.string[from..].find(end_char) {
            Some(end_idx) => {
                let end_idx = end_idx + from;
                let result = &self.string[from..end_idx];
                self.string = &self.string[end_idx+1..];
                Some(result.trim_start())
            },
            None => {
                let result = &self.string[from..];
                self.string = "";
                Some(result.trim())
            }
        }

    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_args() {
        let mut split = Split::from_str("ロリ が 好き");
        assert_eq!(Some("ロリ"), split.next_arg());
        assert_eq!(Some("が"), split.next_arg());
        assert_eq!(Some("好き"), split.next_arg());
        assert_eq!(None, split.next_arg());
    }

    #[test]
    fn test_quoted_args() {
        let mut split = Split::from_str("ロリ  'が が'  好き");
        assert_eq!(Some("ロリ"), split.next_arg());
        assert_eq!(Some("が が"), split.next_arg());
        assert_eq!(Some("好き"), split.next_arg());
        assert_eq!(None, split.next_arg());
    }

    #[test]
    fn test_single_quoted_args() {
        let mut split = Split::from_str("'ロリ が 好き'");
        assert_eq!(Some("ロリ が 好き"), split.next_arg());
        assert_eq!(None, split.next_arg());
    }

    #[test]
    fn test_wrong_quoted_args() {
        let mut split = Split::from_str("foo 'bar test\" baz");
        assert_eq!(Some("foo"), split.next_arg());
        assert_eq!(Some("bar test\" baz"), split.next_arg());
        assert_eq!(None, split.next_arg());
    }

    #[test]
    fn test_several_quoted_args() {
        let mut split = Split::from_str("foo 'bar test' \"baz buzz\"");
        assert_eq!(Some("foo"), split.next_arg());
        assert_eq!(Some("bar test"), split.next_arg());
        assert_eq!(Some("baz buzz"), split.next_arg());
        assert_eq!(None, split.next_arg());

    }
}
