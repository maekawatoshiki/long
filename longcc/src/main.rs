use long_ir::{decl::Decl, Context};
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
        .expect("failed to parse program")
        .into_iter()
        .next()
        .expect("the program is empty")
        .inner;

    // Lower to IR
    let ctx = Context::new();
    let mut ctx = ast2ir::LowerCtx::new(&ctx);
    let decl = ast2ir::lower_decl(&mut ctx, &toplevel).expect("failed to lower AST");

    // Lower to Clif
    let mut ctx = ir2clif::LowerCtx::new();
    let _ = ir2clif::lower_funcdef(
        &mut ctx,
        match decl {
            Decl::FuncDef(funcdef) => funcdef,
            _ => todo!(),
        },
    )
    .expect("failed to lower IR");

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