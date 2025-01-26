use std::{collections::HashMap, iter::Peekable, sync::LazyLock};

use logos::Logos;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, LitStr, Type};

#[derive(logos::Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")]
enum Token<'a> {
    #[token("struct")]
    Struct,
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
        ("float2", "[f32; 2]"),
        #[cfg(not(feature = "glam"))]
        ("float3", "[f32; 3]"),
        #[cfg(not(feature = "glam"))]
        ("float4", "[f32; 4]"),
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
    assert_eq!(lexer.next().unwrap()?, Token::Struct);

    let mut fields = Vec::new();

    let name = match lexer.next().unwrap()? {
        Token::Ident(ident) => ident,
        other => panic!("{:?}", other),
    };

    match lexer.next().unwrap()? {
        Token::BraceOpen => {}
        Token::Colon => {
            // skip inheritance
            while let Some(token) = lexer.next() {
                if token == Ok(Token::BraceOpen) {
                    break;
                }
            }
        }
        other => panic!("{:?}", other),
    }

    while let Some(token) = lexer.next() {
        match token? {
            Token::Ident(ty) => {
                let is_pointer = match lexer.peek() {
                    Some(Ok(Token::Pointer)) => {
                        let _ = lexer.next().unwrap()?;
                        true
                    }
                    _ => false,
                };
                let name = match lexer.next().unwrap()? {
                    Token::Ident(ident) => ident,
                    other => panic!("{:?}", other),
                };

                match lexer.next().unwrap().unwrap() {
                    Token::Semicolon => {
                        fields.push((ty, is_pointer, name));
                    }
                    Token::ParensOpen => {
                        while let Some(token) = lexer.next() {
                            if token == Ok(Token::ParensClose) {
                                break;
                            }
                        }
                        assert_eq!(lexer.next().unwrap()?, Token::BraceOpen);
                        while let Some(token) = lexer.next() {
                            if token == Ok(Token::BraceClose) {
                                break;
                            }
                        }
                        if lexer.peek() == Some(&Ok(Token::Semicolon)) {
                            let _ = lexer.next().unwrap();
                        }
                    }
                    other => panic!("{:?}", other),
                }
            }
            Token::BraceClose => {
                if lexer.peek() == Some(&Ok(Token::Semicolon)) {
                    let _ = lexer.next().unwrap();
                }
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

    while let Some(token) = lexer.peek() {
        match token {
            Ok(Token::Struct) => {
                let slang_struct = parse_struct(&mut lexer).unwrap();

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
                        syn::parse(
                            {
                                if *is_pointer {
                                    "u64"
                                } else {
                                    TYPE_CONVERSION[ty]
                                }
                            }
                            .parse()
                            .unwrap(),
                        )
                        .unwrap()
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
            _ => {
                let _ = lexer.next().unwrap();
            }
        }
    }

    ret.into()
}

#[proc_macro]
pub fn slang_include(input: TokenStream) -> TokenStream {
    let string = parse_macro_input!(input as LitStr).value();
    let file_contents = std::fs::read_to_string(string.as_str()).unwrap();

    slang_struct(file_contents.parse().unwrap())
}
