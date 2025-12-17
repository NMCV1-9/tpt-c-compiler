-- Remember that your LUA_PATH and LUA_CPATH environment variables have been configured

local serpent = require("serpent")
local lexer = require("lexer")
local parser = require("parser")
local irv = require("ir")
local codegen = require("codegen")
local type_checker = require("type_checker")
local symbol_table = require("symbol_table")

local type_checked_ast = type_checker:type_check(parser.parse(lexer.lex([[
/*void fancy_print(char * str, ...) {
    char c;
    int arg_index = 0;
    while(c = *str) {
        if(c == '%'){
            c = *(++str);
            if(c == 'd') {
                __print_signed_int(va_args[arg_index++]);
            }else if(c == 's') {
                printf(va_args[arg_index++]);
            }
        } else if(c =='\'){
            c = *(++str);
            if(c == 'n') {
                putchar((char)1);
            }
        }else{
            putchar(c);
        }
        str++;
    }

}*/
int main() {
    register int i = 101;
}
    ]])), symbol_table)

local c = codegen:generate(irv:generate_ir_code(type_checked_ast, symbol_table), symbol_table)

local file = io.open("main.asm", "w")
file:write(c)
file:close()

