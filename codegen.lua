local Operand = require("operand")
local serpent = require("serpent")
local CodeGen = {
    size=2047,
    global_addr=1,
    symbol_table={},
    available_registers={},
    current_method="!global",
    register_count=22,
    ir = nil
    
}

for i=CodeGen.register_count, 1, -1 do
    table.insert(CodeGen.available_registers, i)
end


function CodeGen:as_memory(operand)
    if(operand.type == "g") then
        return self:as_global(operand)
    elseif(operand.type == "l") then
        return self:as_stack(operand)
    elseif(operand.type == "p") then
        return self:as_parameter(operand)
    else
        return self.as_reg(operand)
    end
end

function CodeGen:as_parameter(operand)
    return "base_pointer, " .. (self.current_method.local_size + operand.value + 2) -- +2 for the return address and base pointer
end

function CodeGen:as_global(operand)
    return self.global_addr + operand.value
end

function CodeGen:as_stack(operand)
    return "base_pointer, " .. (operand.value + 1)
end

function CodeGen.as_reg(operand)
    if(operand.type == "r") then
        if(type(operand.value) == "number") then
            return "r" .. operand.value
        else
            return operand.value
        end
    else
        return "r" .. operand.value
    end
end

function CodeGen:emit_get_address(symbol, dest)
    reg = CodeGen.as_reg(dest)
    if(symbol.type == "g") then
        return string.format("mov %s, %s", reg, self.global_addr + symbol.value)
    elseif(symbol.type == "p") then
        return string.format("mov %s, %s\nadd %s, %s", reg, self.current_method.local_size + 2 + symbol.value, reg, "base_pointer")
    elseif(symbol.type == "l") then
        return string.format("mov %s, %s\nadd %s, %s", reg, symbol.value + 1, reg, "base_pointer")
    elseif(symbol.type == "pr") then
        return string.format("mov %s, %s", reg, self.as_reg(symbol))
    else
        error("Invalid symbol type")
    end
end

CodeGen.emission_map = {
    ["call"]=function(c) return string.format("%s %s", c.type, c.target.type == "i" and c.target.value or CodeGen.as_reg(c.target)) end,
    ["st"]=function(c) return string.format("%s %s, %s", c.type, CodeGen.as_reg(c.source), CodeGen:as_memory(c.dest)) end,
    ["ld"]=function(c) return string.format("%s %s, %s", c.type, CodeGen.as_reg(c.dest), CodeGen:as_memory(c.source)) end,
    ["push"]=function(c) return string.format("%s %s", c.type, CodeGen.as_reg(c.target)) end,
    ["pop"]=function(c) return string.format("%s %s", c.type, CodeGen.as_reg(c.target)) end,
    ["ret"]=function(c) return c.type end,
    ["label"]=function(c) return c.target.value..":" end,
    ["cmp"]=function(c) return string.format("%s %s, %s", c.type, CodeGen.as_reg(c.first), c.second.type == "i" and c.second.value or CodeGen.as_reg(c.second)) end,
    ["!get_address"]=function(c) return CodeGen:emit_get_address(c.target, c.dest) end,
    ["nop"]=function(c) return c.type end
}


setmetatable(CodeGen.emission_map, {
    __index=function(t, x)
                return function(c) 
                    if(string.sub(c.type, 1, 1) == "j") then
                        return string.format("%s %s", c.type, c.target.value)
                    else
                        return string.format("%s %s, %s", c.type, CodeGen.as_reg(c.dest), c.source.type == "i" and c.source.value or CodeGen.as_reg(c.source)) 
                    end
                end
    end                                              
})

function CodeGen:build_global_data_section()
    if self.ir == nil or self.ir.global == 0 then
        return ""
    end

    local data_section = {}
    for i=0, self.ir.global - 1 do
        if self.ir.global_data[i] ~= nil then
            table.insert(data_section, self.ir.global_data[i])
        else
            table.insert(data_section, 0)
        end
    end

    return "dw " .. table.concat(data_section, ", ")
end

function CodeGen:generate(code, symbol_table)
    self.symbol_table = symbol_table
    self.ir = code
    gen = [[
%include "common"

%define return_reg r31
%define stack_pointer r30
%define base_pointer r29
%define term_reg r28
%define return_addr_reg r27


%macro push thing
    subs stack_pointer, 1
    st thing, stack_pointer
%endmacro

%macro pop thing
    ld thing, stack_pointer
    adds stack_pointer, 1
%endmacro

%macro call thing
    push return_addr_reg
    jmp return_addr_reg, thing
%endmacro

%macro ret
    mov r2, return_addr_reg
    pop return_addr_reg
    jmp r2
%endmacro

%macro mull x, y
    mul x, x, y
%endmacro

jmp init
global_data_section:
    ]] .. self:build_global_data_section() .. [[

init:
    mov term_reg, 0x9F80
                              
    ld r0, term_reg                
    mov r1, { 11 5 << }
    st r1, term_reg, 0x42
    mov r1, { 7 5 << }
    st r1, term_reg, 0x43
    mov r17, 0x1000
    st r17, term_reg, 0x44
    mov r17, 0xF
    st r17, term_reg, 0x46
    mov r17, 1
    st r17, term_reg, 0x45

start:
    mov stack_pointer,]] .. self.size .. "\n" -- the stack decrements before storing a value, so the stack will initially overlap with the global storage

    -- handle global code
    self:allocate_registers(code.tac["!global"])
    for i, c in ipairs(code.tac["!global"]) do
        gen = gen .. "\t" .. self.emission_map[c.type](c) .. "\n"
        
    end

    code.tac["!global"] = nil
    if(code.tac["main"] ~= nil) then
        gen = gen .. "\tjmp main\n"
    else
        gen = gen .. "\thlt\n"
    end
    -- handle method code
    for method_id, c in pairs(code.tac) do
        self.current_method = self.symbol_table.get_symbol(method_id, symbol_table.ordinary)
        -- emit prologue
        method = self.symbol_table.get_symbol(method_id, symbol_table.ordinary)
        gen = gen .. method_id .. ":\n"
        gen = gen .. "\tsub stack_pointer, " .. method.local_size .. "\n"
        gen = gen .. "\tpush base_pointer\n"
        gen = gen .. "\tmov base_pointer, stack_pointer\n"
        -- emit body
        self:allocate_registers(c)
        for i, c in ipairs(c) do
            gen = gen .. "\t" .. self.emission_map[c.type](c) .. "\n"
            
        end
        -- emit epilogue
        gen = gen .. ".exit_" .. method_id .. ":\n"
        gen = gen .. "\tpop base_pointer\n"
        gen = gen .. "\tadd stack_pointer, " .. method.local_size .. "\n"
        if(method_id ~= "main") then
            gen = gen .. "\tret\n"
        else
            gen = gen .. "\thlt\n"
        end
    end

    -- print num + other util functions
    built_ins =string.gsub([[
__print_unsigned_int:
	test %1, %1
	jnz .__print_unsigned_int_not_zero
	mov %1, '0'
	st %1, term_reg, 0x25
	jmp .__print_unsigned_int_exit
.__print_unsigned_int_not_zero:
	mov %2, 4		; p = 4
.__print_unsigned_int_fixed_point:
	mulh %3, %1, 52429	; q = (n * 52429) >> 16
	shr %3, 3		; q >>= 3
	mul %4, %3, 10		; d*q
	sub %1, %4		; remainder = n - d*q
	st %1, %2, .__print_unsigned_int_buf		
	sub %2, 1		; p--;
	movf %1, %3		; n = q
	jnz .__print_unsigned_int_fixed_point

	add %2, 1
.__print_unsigned_int_print_int:
	ld %1, %2, .__print_unsigned_int_buf
	add %1, '0'
	st %1, term_reg, 0x25
	add %2, 1
	cmp %2, 5
	jne .__print_unsigned_int_print_int
	
.__print_unsigned_int_exit:
	ret
.__print_unsigned_int_buf:
	dw 0, 0, 0, 0, 0

__print_signed_int:
    cmp %1, 0
    jge .__print_signed_int_not_negative
    mov %2, '-'
    st %2, term_reg, 0x25
	xor %1, 65535
    add %1, 1
.__print_signed_int_not_negative:
    call __print_unsigned_int
    ret
    
printf:
.printf_loop:
    ld %2, %1
    test %2, %2
    jz .printf_exit
    st %2, term_reg, 0x25
    add %1, 1
    jmp .printf_loop
.printf_exit:
    ret

putchar:
    st %1, term_reg, 0x25
    ret

getchar:
    ld return_reg, term_reg
    test return_reg, return_reg
    jz getchar
    ret

]], "%%(%d+)", {["1"]="r23", ["2"]="r24", ["3"]="r25", ["4"]="r26"})

    return gen .. built_ins
end

function CodeGen:allocate_registers(tac)
    local intervals = {}
    for i, c in ipairs(tac) do
        for j, operand in pairs(c) do
            if(operand.type == "t" or operand.type == "pr") then
                
                if(intervals[operand.value] == nil) then
                    -- overwrite the metatable to sort
                    intervals[operand.value] = {start=i, last_use=i}
                else
                    intervals[operand.value].last_use = i
                end
            end
        end
    end

    local events = {}
    for reg_id, interval in pairs(intervals) do
        table.insert(events, {type="+", value=reg_id, time=interval.start})
        table.insert(events, {type="-", value=reg_id, time=interval.last_use})
    end

    table.sort(events, function(a, b) return a.time < b.time end)

    local register_map = {}

    for i, event in ipairs(events) do
        if(event.type == "+") then
            register_map[event.value] = table.remove(self.available_registers)
        else
            table.insert(self.available_registers, register_map[event.value])
        end
    end

    for i, c in ipairs(tac) do
        for j, operand in pairs(c) do
            if(operand.type == "t" or operand.type == "pr") then
                
                operand.type = "r"
                operand.value = register_map[operand.value]
                
            end
        end
    end
end

return CodeGen