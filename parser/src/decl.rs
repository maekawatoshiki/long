use crate::lexer::traits::LexerLike;
use crate::Parser;
use anyhow::Result;
use long_ast::node::{decl::Decl, Located};

impl<'a, L: LexerLike> Parser<'a, L> {
    /// Parses a declaration sequence.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
    pub(crate) fn parse_decl_seq(&mut self) -> Result<Located<Decl>> {
        // TODO
        self.parse_decl()
    }

    /// Parses a declaration.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.dcl#1>
    pub(crate) fn parse_decl(&mut self) -> Result<Located<Decl>> {
        // TODO
        self.parse_func_def()
    }

    /// Parses a function definition.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.fct.def.general#1>
    fn parse_func_def(&mut self) -> Result<Located<Decl>> {
        // function-definition:
        //     attribute-specifier-seq(opt) decl-specifier-seq(opt) declarator virt-specifier-seq(opt) function-body
        let _decl_specifier_seq = self.parse_decl_specifier_seq();
        todo!()
    }

    /// Parses a decl specifier sequence.
    /// <https://timsong-cpp.github.io/cppwp/n3337/dcl.spec#nt:decl-specifier-seq>
    fn parse_decl_specifier_seq(&mut self) -> Result<Located<Decl>> {
        // TODO
        self.parse_decl_specifier()
    }

    /// Parses a decl specifier.
    fn parse_decl_specifier(&mut self) -> Result<Located<Decl>> {
        todo!()
    }
}
