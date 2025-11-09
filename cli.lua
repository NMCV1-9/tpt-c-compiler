-- Remember that your LUA_PATH and LUA_CPATH environment variables have been configured

local serpent = require("serpent")
local lexer = require("lexer")
local parser = require("parser")
local irv = require("ir")
local codegen = require("codegen")


print(serpent.block(lexer.lex([[
    int str = "wow";
]])))

local c = codegen:generate(irv:generate_ir_code(parser.parse(lexer.lex([[
    
    int x = 8;

    int main() {
        int *y = &x;
        *y = 5 + *y;
        print_num(x);
    }
]]))))

local file = io.open("main.asm", "w")
file:write(c)
file:close()

