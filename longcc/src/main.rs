use long_ir::Context;
use long_lower::{ast2ir, ir2clif};
use long_parser::{lexer::Lexer, Parser};
use std::{fs::File, io::Write, path::PathBuf};
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "longcc", about = "C++ Compiler from Scratch")]
struct Opt {
    #[structopt(name = "FILE", parse(from_os_str))]
    file: PathBuf,
}

fn main() {
    let opt = Opt::from_args();

    // Parse
    let toplevel = Parser::new(&mut Lexer::new_from_file(&opt.file).expect("failed to open file"))
        .parse_program()
        .expect("failed to parse program");

    // Lower to IR
    let ctx = Context::new();
    let mut ctx = ast2ir::LowerCtx::new(&ctx);
    let mut decls = vec![];
    for decl in toplevel {
        decls.push(ast2ir::lower_decl(&mut ctx, &decl.inner).expect("failed to lower AST"));
    }

    // println!("{:#?}", decls);

    // Lower to Clif
    let mut ctx = ir2clif::LowerCtx::new();
    for decl in decls {
        let _ = ir2clif::lower_decl(&mut ctx, decl).expect("failed to lower IR");
    }

    // Generate object code
    let product = ctx.module.finish();
    let obj = product.emit().expect("failed to emit the object code");

    // Write output file
    let mut outname = opt.file;
    outname.set_extension("o");
    File::create(outname)
        .expect("failed to create an output file")
        .write_all(&obj)
        .expect("failed to write the output file");
}
