local lexer = require("lexer")
local parser = require("parser")
local irv = require("ir")
local codegen = require("codegen")
local type_checker = require("type_checker")
local symbol_table = require("symbol_table")
local util = require("util")

local usage = "Usage: lua cli.lua input.c [--output output.asm] [--size total-memory-size] [--term-width width] [--term-height height] [--offset offset]" 
if(#arg < 1) then
    print(usage)
    os.exit(1)
end

local output_name = string.sub(arg[1], 1, #arg[1] - 2) .. ".asm"


for arg_idx = 2, #arg - 1, 2 do
    if arg[arg_idx] == "--offset" then
        local offset = tonumber(arg[arg_idx + 1])
        if not offset then
            error("Invalid offset argument: '"..arg[arg_idx + 1].."'")
        end
        codegen.global_addr = codegen.global_addr + offset
    elseif arg[arg_idx] == "--size" then
        local size = tonumber(arg[arg_idx + 1])
        if(size < 1024) then
            print("[WARNING] Size is less than 1024. Remember that the '--size' argument expects the total memory size, not the number of memory rows.")
        end

        if not size then
            error("Invalid size argument: '"..arg[arg_idx + 1].."'")
        end
        codegen.size = size - 1
    elseif arg[arg_idx] == "--output" then
        output_name = arg[arg_idx + 1]
    elseif arg[arg_idx] == "--term-width" then
        local width = tonumber(arg[arg_idx + 1])
        if not width then
            error("Invalid term-width argument: '"..arg[arg_idx + 1].."'")
        end
        codegen.term_width = width
    elseif arg[arg_idx] == "--term-height" then
        local height = tonumber(arg[arg_idx + 1])
        if not height then
            error("Invalid term-height argument: '"..arg[arg_idx + 1].."'")
        end
        codegen.term_height = height
    end
end

local file = io.open(arg[1], "r")
local code = nil
if(file) then
    code = file:read("*all")
    file:close()
else
    error("Failed to open file")
end


local asm = codegen:generate(irv:generate_ir_code(type_checker:type_check(parser.parse(lexer.lex(code), symbol_table))))
local out_file = io.open(output_name, "w")
out_file:write(asm)
out_file:close()
