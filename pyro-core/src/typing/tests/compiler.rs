use crate::annotate::annotate_decl;
use crate::ast::Tag;
use crate::parser::ParserState;
use crate::sym::Sym;
use crate::tokenizer::Tokenizer;
use crate::{infer_decl, NominalTyping, TypePointer, TypeSystem, STDLIB};

pub struct TestCompiler {
    system: TypeSystem<NominalTyping>,
}

impl TestCompiler {
    pub fn new() -> Self {
        let mut system = TypeSystem::new(NominalTyping::default());

        system.push_scope();
        let var = system.new_generic("'a");
        system.pop_scope();

        system.declare_from_pointer(
            "print",
            &TypePointer::app(
                TypePointer::client(),
                TypePointer::ForAll(
                    false,
                    STDLIB.as_local_scope(),
                    vec!["'a".to_string()],
                    Box::new(TypePointer::Qual(
                        vec![TypePointer::app(TypePointer::show(), var.clone())],
                        Box::new(var),
                    )),
                ),
            ),
        );

        Self { system }
    }

    pub fn load_module(&mut self, source_code: &str) -> eyre::Result<()> {
        let tokenizer = Tokenizer::new(source_code);
        let tokens = tokenizer.tokenize()?;
        let mut parser = ParserState::new(tokens.as_slice());

        loop {
            parser.skip_spaces();
            let pos = parser.pos();
            let decl = parser.parse_decl()?;
            let node = Tag {
                item: decl,
                tag: pos,
            };

            let decl = infer_decl(&mut self.system, node)?;
            annotate_decl(&mut self.system, decl)?;

            parser.skip_spaces();

            if parser.look_ahead().item == Sym::EOF {
                break;
            }
        }

        Ok(())
    }

    pub fn compile(&self, source_code: &str) -> eyre::Result<()> {
        let mut type_system = self.system.clone();

        crate::parse(&mut type_system, source_code)?;

        Ok(())
    }
}
