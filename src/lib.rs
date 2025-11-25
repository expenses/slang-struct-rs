use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
    sync::LazyLock,
};

use logos::Logos;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, LitStr, Type};

#[derive(logos::Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")]
enum Token<'a> {
    #[token("public")]
    Public,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("property")]
    Property,
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token(";")]
    Semicolon,
    #[regex("[a-zA-Z_0-9]+")]
    Ident(&'a str),
    #[token("*")]
    Pointer,
    #[token("(")]
    ParensOpen,
    #[token(")")]
    ParensClose,
    #[token(":")]
    Colon,
    #[token("<")]
    OpenGeneric,
    #[token(">")]
    CloseGeneric,
}

const TYPE_CONVERSION: LazyLock<HashMap<&str, &str>> = LazyLock::new(|| {
    HashMap::from([
        ("int8_t", "i8"),
        ("uint8_t", "u8"),
        ("int16_t", "i16"),
        ("uint16_t", "u16"),
        ("int32_t", "i32"),
        ("uint32_t", "u32"),
        ("int", "i32"),
        ("uint", "u32"),
        ("int64_t", "i64"),
        ("uint64_t", "u64"),
        ("float", "f32"),
        #[cfg(not(feature = "glam"))]
        ("float4x4", "[f32; 16]"),
        #[cfg(feature = "glam")]
        ("float2", "glam::Vec2"),
        #[cfg(feature = "glam")]
        ("float3", "glam::Vec3"),
        #[cfg(feature = "glam")]
        ("float4", "glam::Vec4"),
        #[cfg(feature = "glam")]
        ("float4x4", "glam::Mat4"),
    ])
});

struct SlangStruct<'a> {
    name: &'a str,
    fields: Vec<(&'a str, bool, &'a str)>,
}

type PeekableLexer<'a> = Peekable<logos::Lexer<'a, Token<'a>>>;

fn parse_struct<'a>(lexer: &mut PeekableLexer<'a>) -> Result<SlangStruct<'a>, ()> {
    assert_eq!(lexer.next().expect("struct")?, Token::Struct);

    let mut fields = Vec::new();

    let name = match lexer.next().expect("struct name")? {
        Token::Ident(ident) => ident,
        other => panic!("Expected struct name: {:?}", other),
    };

    match lexer.next().expect("struct next")? {
        Token::BraceOpen => {}
        Token::Colon => {
            // skip inheritance
            while let Some(token) = lexer.next() {
                if token == Ok(Token::BraceOpen) {
                    break;
                }
            }
        }
        other => panic!("Expected struct brace open: {:?}", other),
    }

    let consume_inner_braces = |lexer: &mut PeekableLexer| {
        let mut brace_level = 1;
        while let Some(token) = lexer.next() {
            match token {
                Ok(Token::BraceOpen) => brace_level += 1,
                Ok(Token::BraceClose) => {
                    brace_level -= 1;
                    if brace_level == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }
    };

    let consume_optional_semicolons = |lexer: &mut PeekableLexer| {
        while lexer.peek() == Some(&Ok(Token::Semicolon)) {
            let _ = lexer.next().expect("consume optional semicolons");
        }
    };

    while let Some(token) = lexer.next() {
        match token? {
            Token::Public => {}
            Token::Property => {
                let _ty = match lexer.next().expect("property ty")? {
                    Token::Ident(ident) => ident,
                    other => panic!("Expected property type, got {:?}", other),
                };
                let _name = match lexer.next().expect("property name")? {
                    Token::Ident(ident) => ident,
                    other => panic!("Expected property name, got {:?}", other),
                };
                assert_eq!(
                    lexer.next().expect("property brace open")?,
                    Token::BraceOpen
                );
                consume_inner_braces(lexer);
                consume_optional_semicolons(lexer);
            }
            Token::Ident(ty) => {
                if let Some(Ok(Token::OpenGeneric)) = lexer.peek() {
                    let _ = lexer.next().unwrap()?;
                    assert!(matches!(lexer.next(), Some(Ok(Token::Ident(_)))));
                    while lexer.peek() == Some(&Ok(Token::Semicolon)) {
                        let _ = lexer.next().expect("consume optional semicolons");
                        assert!(matches!(lexer.next(), Some(Ok(Token::Ident(_)))));
                    }
                    assert!(matches!(lexer.next(), Some(Ok(Token::CloseGeneric))));
                }

                let is_pointer = match lexer.peek() {
                    Some(Ok(Token::Pointer)) => {
                        let _ = lexer.next().unwrap()?;
                        true
                    }
                    _ => false,
                };
                let name = match lexer.next().expect("ident name")? {
                    Token::Ident(ident) => ident,
                    other => panic!(
                        "Expected field name in {} ({:?}, {:?}, {}): {:?}",
                        name, fields, ty, is_pointer, other
                    ),
                };

                match lexer.next().expect("ident next").expect("ident next next") {
                    Token::Semicolon => {
                        fields.push((ty, is_pointer, name));
                    }
                    Token::ParensOpen => {
                        while let Some(token) = lexer.next() {
                            if token == Ok(Token::ParensClose) {
                                break;
                            }
                        }
                        assert_eq!(lexer.next().expect("parens open")?, Token::BraceOpen);
                        consume_inner_braces(lexer);
                        consume_optional_semicolons(lexer);
                    }
                    other => panic!("Unexpected symbol in field: {:?}", other),
                }
            }
            Token::BraceClose => {
                consume_optional_semicolons(lexer);
                break;
            }
            other => panic!("{:?}", other),
        }
    }

    Ok(SlangStruct { name, fields })
}

#[proc_macro]
pub fn slang_struct(input: TokenStream) -> TokenStream {
    let string = &input.to_string();

    let mut lexer = Token::lexer(string).peekable();

    let mut ret = proc_macro2::TokenStream::new();

    let mut structs = Vec::new();
    let mut enums = HashSet::new();

    while let Some(token) = lexer.peek() {
        match token {
            Ok(Token::Struct) => {
                let slang_struct = parse_struct(&mut lexer).expect("slang struct");
                structs.push(slang_struct);
            }
            Ok(Token::Enum) => {
                let _ = lexer.next().expect("enum");
                let name = match lexer.next().expect("enum name").expect("enum name inner") {
                    Token::Ident(ident) => ident,
                    other => panic!("Expected ident for enum, got: {:?}", other),
                };
                enums.insert(name);
            }
            _ => {
                let _ = lexer.next().expect("enum other");
            }
        }
    }

    for slang_struct in structs {
        let name = Ident::new(&slang_struct.name, proc_macro2::Span::call_site());

        let names: Vec<Ident> = slang_struct
            .fields
            .iter()
            .clone()
            .map(|(_, _, name)| Ident::new(name, proc_macro2::Span::call_site()))
            .collect();
        let types: Vec<Type> = slang_struct
            .fields
            .iter()
            .clone()
            .map(|(ty, is_pointer, _)| {
                let get_ty = || -> String {
                    if *is_pointer {
                        "u64".to_string()
                    } else if enums.contains(ty) {
                        "u32".to_string()
                    } else if let Some(ty) = TYPE_CONVERSION.get(ty) {
                        ty.to_string()
                    } else if ty.ends_with(['2', '3', '4']) {
                        let count = &ty[ty.len() - 1..];
                        let prefix = &ty[..ty.len() - 1];
                        return if let Some(ty) = TYPE_CONVERSION.get(prefix) {
                            format!("[{}; {}]", ty, count)
                        } else {
                            // return original.
                            ty.to_string()
                        };
                    } else {
                        ty.to_string()
                    }
                };

                syn::parse(get_ty().parse().unwrap()).unwrap()
            })
            .collect();

        ret.extend(quote!(#[repr(C)]));
        ret.extend(if cfg!(feature = "bytemuck") {
            quote!(#[derive(Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)])
        } else {
            quote!(#[derive(Clone, Copy, Default)])
        });

        ret.extend(quote! {
            pub struct #name {
                #(#names: #types),*
            }
        });
    }

    ret.into()
}

#[proc_macro]
pub fn slang_include(input: TokenStream) -> TokenStream {
    let string = parse_macro_input!(input as LitStr).value();
    let file_contents = std::fs::read_to_string(string.as_str()).unwrap();

    slang_struct(file_contents.parse().unwrap())
}
