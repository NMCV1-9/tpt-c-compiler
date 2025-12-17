local Node = require('node')
local Token = require('token')
local Operand = require('operand')
local util = require('util')
local Type = require('type')
local serpent = require('serpent')

-- Intermediate representation code generation

local IRVisitor = { tac = {["!global"]={}},
                    temp = 0,
                    global = 0,
                    label = 0,
                    global_data = {},
                    types = {["INT"]=1, ["CHAR"]=1, ["VOID"]=1, ["POINTER"]=1},
                    global_method = {id = "!global"},
                    loop_labels = {}
                    }
IRVisitor.method = IRVisitor.global_method
local operand = {
    l=function(s, method) return Operand:new("l", IRVisitor:next_stack(s, method or IRVisitor.method)) end, -- local variable
    p=function(v) return Operand:new("p", v) end,                   -- parameter
    g=function(s, l) return Operand:new("g", IRVisitor:next_global(s, l)) end, -- global variable
    pr=function() return Operand:new("pr", IRVisitor:next_temp()) end,
    i=function(v) return Operand:new("i", v) end,                      -- immediate
    t=function() return Operand:new("t", IRVisitor:next_temp()) end,    -- temporary    
    r=function(v) return Operand:new("r", v) end,                        -- binded register
    lb=function() return Operand:new("i", string.format(".label_%d", IRVisitor:next_label())) end
}
IRVisitor.standard_function_arguments = {operand.r("r23")}
IRVisitor.RETURN_REG = operand.r("return_reg")
IRVisitor.STACK_POINTER = operand.r("stack_pointer")
IRVisitor.BASE_POINTER = operand.r("base_pointer")
IRVisitor.__index = IRVisitor

function IRVisitor:next_label()
    local temp = self.label
    self.label = self.label + 1
    return temp
end

function IRVisitor:next_stack(size, method)
    assert(method.local_size ~= nil, "Method local size is nil")
    local result = method.local_size
    method.local_size = result + size
    return result
end

function IRVisitor:next_temp()
    self.temp = self.temp + 1
    return self.temp
end

function IRVisitor:next_global(size, l)
    local temp = self.global
    -- Update global_data table with the initial data list
    if l then
        local data_entry = {idx=temp, list=l, next_entry=nil}

        if(self.global_data ~= nil) then
            self.global_data.next_entry = data_entry
        end

        self.global_data = data_entry
    end

    self.global = temp + size
    return temp
end

function IRVisitor:register_global_word(data, start)
    self.global_data[start.value] = data
end

function IRVisitor:sizeof(type)
    local size = 1
    if (type == nil) then
        return size
    end
    while(type ~= nil and type.kind == Type.KINDS["ARRAY"]) do
        size = size * type.length
        type = type.points_to
    end

    if(type.kind == Type.KINDS["STRUCT"] or type.kind == Type.KINDS["UNION"]) then
        return size * type.size
    end

    assert(type ~= nil, "Base type is nil")
    return self.types[Type.INVERTED_KINDS[type.kind]] * size
end


function IRVisitor:generate_ir_code(ast, symbol_table)
    local NODE_TYPES = Node.NODE_TYPES
    local TOKEN_TYPES = Token.TOKEN_TYPES
    local PLACE_TYPES = Operand.PLACE_TYPES
    local tac = self.tac
    local global_method = self.global_method
    local get_symbol = symbol_table.get_symbol
    local set_symbol = symbol_table.set_symbol
    local new_scope = symbol_table.new_scope
    local exit_scope = symbol_table.exit_scope

    local node_check = Node.node_check

    memory_operands = {["l"]=1,["p"]=2,["g"]=3, ["pr"]=4}
    register_operands = {["t"]=1, ["r"]=2}
    
    function emit_program(n)
        for _, child in ipairs(n) do
            emit_declaration(child)
        end
    end


    function load_operand_into_register(place)
        -- for idempotency
        if(register_operands[place.type]) then
            return place
        end


        
        if(place.type == "pr") then
            local new_place = operand.t()
            table.insert(tac[self.method.id], {type = "ld", source=place, dest=new_place})
            return new_place
        end

        local next_reg = operand.t()
        emit_move(place, next_reg)

        return next_reg
    end

    

    function initialize_word(value, place)
        if(place.type == "g") then
            self:register_global_word(value, place)
        else
            emit_move(operand.i(value), place)
        end
    end

    function emit_static_initializer(n, start)
        local start = Operand:new(start.type, start.value)
        -- n is the initializer: either of type initializer_list or initializer
        if(node_check(n, "INITIALIZER")) then
            local element = n.value
            if(node_check(element, "INT")) then
                initialize_word(element.value, start)
            elseif(node_check(element, "CHARACTER")) then
                initialize_word(element.value, start)
            elseif(node_check(element, "STRING_LITERAL")) then
                register_string_literal(element, start)
            else           
                emit_assignment_expression(element)
                emit_move(element.place, start)
            end
        elseif(node_check(n, "INITIALIZER_LIST")) then
            for i, child in ipairs(n) do
                if(n.value_type.kind == Type.KINDS["ARRAY"]) then
                    child.value_type = n.value_type.points_to
                elseif(n.value_type.kind == Type.KINDS["STRUCT"]) then
                    child.value_type = n.value_type.members[i].type
                elseif(n.value_type.kind == Type.KINDS["UNION"]) then
                    child.value_type = Type.base("VOID")
                end
                emit_static_initializer(child, start)
                start = Operand:new(start.type, start.value + self:sizeof(child.value_type))
            end
        end
    end

    function register_string_literal(n, start)
        for i = 1, n.value_type.length do 
            local char = 0
            if(i <= #n.value) then
                char = string.format("'%s'", string.sub(n.value, i, i))
            end

            self:register_global_word(char, start)
            start = Operand:new(start.type, start.value + 1)
        end
    end

    function static_allocate_place(size)
        size = size or 1
        if(self.method.id == "!global") then
            return operand.g(size)
        else
            return operand.l(size)
        end
    end

    function compute_struct_layout(type)
        local offset = 0
        for i, member in ipairs(type.members) do
            member.offset = offset
            offset = offset + self:sizeof(member.type)
        end
        type.size = offset
        return type
    end

    function compute_union_layout(type)
        local size = 0
        for i, member in ipairs(type.members) do
            member.offset = 0
            size = math.max(size, self:sizeof(member.type))
        end
        type.size = size
        return type
    end

    function emit_declaration(n)
        assert(n.value_type ~= nil, "Value type is nil")
        --handle_name_definition_conflict(n.id)
        if(n.handle.type.kind == Type.KINDS["STRUCT"] and n.specifier.type_specifier.kind.declaration) then
            compute_struct_layout(n.handle.type)
        elseif(n.handle.type.kind == Type.KINDS["UNION"] and n.specifier.type_specifier.kind.declaration) then -- make sure the layout is only computed when the union is declared
            compute_union_layout(n.handle.type)
        end

        if(not n.declarator) then
            return
        end

        if(n.value_type.kind ~= Type.KINDS["FUNCTION"]) then
            local place = nil
            if(n.initializer) then
                local initializer_place = nil
                if(n.initializer.value and node_check(n.initializer.value, "STRING_LITERAL")) then
                    initializer_place = operand.g(n.initializer.value_type.length) -- string literals are stored in global memory
                else
                    initializer_place = static_allocate_place(self:sizeof(n.initializer.value_type))
                end
                
                emit_static_initializer(n.initializer, initializer_place)
                if((node_check(n.initializer, "INITIALIZER_LIST") or node_check(n.initializer.value, "STRING_LITERAL")) and n.value_type.kind == Type.KINDS["POINTER"]) then
                    place = static_allocate_place(1) -- pointer to object
                    if(self.method.id == "!global" or node_check(n.initializer.value, "STRING_LITERAL")) then -- String literal must be included even for local variables
                        initialize_word(initializer_place.value+1, place) -- +1 is to offset the initial jmp start instruction (poor design, might fix later)
                    else
                        local next_reg = operand.t()
                        table.insert(tac[self.method.id], {type="!get_address", target=n.handle.place, dest=next_reg}) 
                        emit_move(next_reg, n.handle.place)
                    end
                else
                    place=initializer_place -- object itself
                end
            else
                if(n.specifier.storage_class and n.specifier.storage_class.kind == "register") then
                    print("YES")
                    place = operand.t()
                else
                    place=static_allocate_place(self:sizeof(n.value_type)) -- no initializer
                end
            end
            n.handle.place = place
        else
            -- function definition
            assert(self.method.id == "!global", "Nested function definitions are not supported")

            n.handle.place = operand.i(n.id.id)
            n.handle.local_size = 0
            n.handle.id = n.id.id
            self.method = n.handle
            tac[self.method.id] = {}

            new_scope(self.method.id)

            
            for i, p in ipairs(n.declarator.direct_declarator.parameter_list or {}) do
                p.handle.place = operand.p(i-1)
            end

            if(n.block) then
                emit_block(n.block)
            else
                
            end
            exit_scope()
            self.method = global_method

        end
    end

    function emit_expression(n)
        for i = 1, #n do
            emit_assignment_expression(n[i])
        end
        
        n.place = n[#n].place
    end

    function emit_assignment_expression(n)
        if (n.lhs) then 
            emit_ternary_expression(n.lhs)
            -- n.lhs.place.type = "g"
            emit_assignment_expression(n.rhs)
            n.place = emit_move(n.rhs.place, n.lhs.place) -- emit_move will return the source for optimization reasons

        else
            emit_ternary_expression(n)
            -- n.place is assumed to have been updated in either emit_ternary_expression or somewhere in the chain of function calls
        end

    end

    function emit_ternary_expression(n)
        if(node_check(n, "TERNARY")) then
            emit_logical_or_expression(n.condition)
            n.place = load_operand_into_register(n.condition.place)
            table.insert(tac[self.method.id], {type="cmp", first=n.place, second=operand.i(0)})
            local false_label = operand.lb()
            local end_label = operand.lb()
            table.insert(tac[self.method.id], {type="je", target=false_label})
            emit_assignment_expression(n.true_case)
            emit_move(n.true_case.place, n.place) -- should n.place be a t only?
            table.insert(tac[self.method.id], {type="jmp", target=end_label})
            table.insert(tac[self.method.id], {type="label", target=false_label})
            emit_logical_or_expression(n.false_case)
            emit_move(n.false_case.place, n.place)
            table.insert(tac[self.method.id], {type="label", target=end_label})
        else
            emit_logical_or_expression(n)
        end
    end

    function emit_logical_or_expression(n)
        if(node_check(n, "LOGICAL_OR_EXPRESSION")) then
            local true_label = operand.lb()
            local false_label = operand.lb()
            local end_label = operand.lb()
            for i = 1, #n do
                emit_logical_and_expression(n[i])
                n.place = load_operand_into_register(n[i].place)
                table.insert(tac[self.method.id], {type="cmp", first=n.place, second=operand.i(0)})
                table.insert(tac[self.method.id], {type="jne", target=true_label})
            end
            table.insert(tac[self.method.id], {type="label", target=false_label})
            emit_move(operand.i(0), n.place)
            table.insert(tac[self.method.id], {type="jmp", target=end_label})
            table.insert(tac[self.method.id], {type="label", target=true_label})
            emit_move(operand.i(1), n.place)
            table.insert(tac[self.method.id], {type="label", target=end_label})
            
        else
            emit_logical_and_expression(n)
        end
    end

    function emit_logical_and_expression(n)
        if(node_check(n, "LOGICAL_AND_EXPRESSION")) then
            local true_label = operand.lb()
            local false_label = operand.lb()
            local end_label = operand.lb()
            for i = 1, #n do
                emit_inclusive_or_expression(n[i])
                n.place = load_operand_into_register(n[i].place)
                table.insert(tac[self.method.id], {type="cmp", first=n.place, second=operand.i(0)})
                table.insert(tac[self.method.id], {type="je", target=false_label})
            end
            table.insert(tac[self.method.id], {type="label", target=true_label})
            emit_move(operand.i(1), n.place)
            table.insert(tac[self.method.id], {type="jmp", target=end_label})
            table.insert(tac[self.method.id], {type="label", target=false_label})
            emit_move(operand.i(0), n.place)
            table.insert(tac[self.method.id], {type="label", target=end_label})
        else
            emit_inclusive_or_expression(n)
        end
    end

    function emit_inclusive_or_expression(n)
        if(node_check(n, "INCLUSIVE_OR_EXPRESSION")) then
            emit_inclusive_xor_expression(n[1])
            n.place = load_operand_into_register(n[1].place)
            for i = 2, #n do
                emit_inclusive_xor_expression(n[i])
                local next_reg = load_operand_into_register(n[i].place)
                table.insert(tac[self.method.id], {type="or", source=next_reg, dest=n.place})
            end
        else
            emit_inclusive_xor_expression(n)
        end
    end

    function emit_inclusive_xor_expression(n)
        if(node_check(n, "INCLUSIVE_XOR_EXPRESSION")) then
            emit_inclusive_and_expression(n[1])
            n.place = load_operand_into_register(n[1].place)
            for i = 2, #n do
                emit_inclusive_and_expression(n[i])
                local next_reg = load_operand_into_register(n[i].place)
                table.insert(tac[self.method.id], {type="xor", source=next_reg, dest=n.place})
            end
        else
            emit_inclusive_and_expression(n)
        end
    end

    function emit_inclusive_and_expression(n)
        if(node_check(n, "INCLUSIVE_AND_EXPRESSION")) then
            emit_equality_expression(n[1])
            n.place = load_operand_into_register(n[1].place)
            for i = 2, #n do
                emit_equality_expression(n[i])
                local next_reg = load_operand_into_register(n[i].place)
                table.insert(tac[self.method.id], {type="and", source=next_reg, dest=n.place})
            end
        else
            emit_equality_expression(n)
        end
    end

    function emit_equality_expression(n)
        if(node_check(n, "EQUALITY_EXPRESSION")) then
            emit_relational_expression(n[1])
            n.place = load_operand_into_register(n[1].place)
            local next_reg = nil
            for i = 3, #n, 2 do
                emit_relational_expression(n[i])
                
                if(not register_operands[n[i].place.type] and n[i].place.type ~= "i") then
                    if(next_reg == nil) then
                        next_reg = operand.t()
                    end
                    emit_move(n[i].place, next_reg)
                else
                    next_reg = n[i].place
                end
                local false_label = operand.lb()
                local end_label = operand.lb()

                
                table.insert(tac[self.method.id], {type="cmp", first=n.place, second=next_reg})
                if(n[i - 1].type == TOKEN_TYPES['==']) then
                    table.insert(tac[self.method.id], {type="jne", target=false_label})
                else
                    table.insert(tac[self.method.id], {type="je", target=false_label})
                end
                emit_move(operand.i(1), n.place) -- true case
                table.insert(tac[self.method.id], {type="jmp", target=end_label})
                table.insert(tac[self.method.id], {type="label", target=false_label})
                emit_move(operand.i(0), n.place)
                table.insert(tac[self.method.id], {type="label", target=end_label})
            end
        else
            emit_relational_expression(n)
        end
    end

    function emit_relational_expression(n)
        if(node_check(n, "RELATIONAL_EXPRESSION")) then
            emit_shift_expression(n[1])
            n.place = load_operand_into_register(n[1].place)
            local next_reg = nil
            for i = 3, #n, 2 do
                emit_shift_expression(n[i])
                if(not register_operands[n[i].place.type] and n[i].place.type ~= "i") then
                    if(next_reg == nil) then
                        next_reg = operand.t()
                    end
                    emit_move(n[i].place, next_reg)
                else
                    next_reg = n[i].place
                end
                local true_label = operand.lb()
                local end_label = operand.lb()

                
                table.insert(tac[self.method.id], {type="cmp", first=n.place, second=next_reg})
                if(n[i - 1].type == TOKEN_TYPES['<']) then
                    table.insert(tac[self.method.id], {type=n.value_type.signed and "jl" or "jb", target=true_label})
                elseif(n[i - 1].type == TOKEN_TYPES['<=']) then
                    table.insert(tac[self.method.id], {type=n.value_type.signed and "jle" or "jbe", target=true_label})
                elseif(n[i - 1].type == TOKEN_TYPES['>']) then
                    table.insert(tac[self.method.id], {type=n.value_type.signed and "jg" or "ja", target=true_label})
                elseif(n[i - 1].type == TOKEN_TYPES['>=']) then
                    table.insert(tac[self.method.id], {type=n.value_type.signed and "jge" or "jae", target=true_label})
                end
                emit_move(operand.i(0), n.place) -- true case
                table.insert(tac[self.method.id], {type="jmp", target=end_label})
                table.insert(tac[self.method.id], {type="label", target=true_label})
                emit_move(operand.i(1), n.place)
                table.insert(tac[self.method.id], {type="label", target=end_label})
            end
        else
            emit_shift_expression(n)
        end
    end

    function emit_shift_expression(n)
        if(node_check(n, "SHIFT_EXPRESSION")) then
            emit_sum_expression(n[1])
            n.place = load_operand_into_register(n[1].place)
            local next_reg = operand.t()
            for i = 3, #n, 2 do
                emit_sum_expression(n[i])
                if(not register_operands[n[i].place.type]) then
                    emit_move(n[i].place, next_reg)
                else
                    next_reg = n[i].place
                end
                if(n[i - 1].type == TOKEN_TYPES['<<']) then
                    table.insert(tac[self.method.id], {type="shl", source=next_reg, dest=n.place})
                else -- logical right shift
                    table.insert(tac[self.method.id], {type="shr", source=next_reg, dest=n.place})
                end
            end
        else
            emit_sum_expression(n)
        end
    end


    function emit_move(source, dest)
        assert(dest.type ~= "i", "destination type cannot be an immediate value")

        source = source.type ~= "pr" and source or load_operand_into_register(source)

        local is_source_mem = memory_operands[source.type]
        local is_dest_mem = memory_operands[dest.type]

        if(source.type == "i" and is_dest_mem) then
            local next_reg = operand.t()
            table.insert(tac[self.method.id], {type="mov", source=source, dest=next_reg})
            source = next_reg -- source is still guaranteed to not be a memory based operand
        end


        if(not is_source_mem and not is_dest_mem) then
            table.insert(tac[self.method.id], {type="mov", source=source, dest=dest})
        elseif(not is_source_mem and is_dest_mem) then
            table.insert(tac[self.method.id], {type="st", source=source, dest=dest})
        elseif(is_source_mem and not is_dest_mem) then
            table.insert(tac[self.method.id], {type="ld", source=source, dest=dest})
        else
            -- source and dest are both memory based operands
            local t = operand.t()
            table.insert(tac[self.method.id], {type="ld", source=source, dest=t})
            table.insert(tac[self.method.id], {type="st", source=t, dest=dest})
        end

        return source
    end


    function emit_block(n)
        for i, s in ipairs(n) do
            emit_statement(s)
        end
    end

    function emit_statement(n)
        if(n.child.type == NODE_TYPES["DECLARATION"]) then
            emit_declaration(n.child)
        elseif(n.child.type == NODE_TYPES["IF"]) then
            emit_if(n.child)
        elseif(n.child.type == NODE_TYPES["RETURN"]) then
            emit_return(n.child)
        elseif(n.child.type == NODE_TYPES["BLOCK"]) then
            emit_block(n.child)
        elseif(n.child.type == NODE_TYPES["FOR"]) then
            emit_for(n.child)
        elseif(n.child.type == NODE_TYPES["BREAK"]) then
            emit_break(n.child)
        elseif(n.child.type == NODE_TYPES["WHILE"]) then
            emit_while(n.child)
        elseif(n.child.type == NODE_TYPES["CONTINUE"]) then
            emit_continue(n.child)
        else
            emit_expression(n.child)
        end
    end

    function emit_if(n)
        emit_expression(n.condition)
        n.place = load_operand_into_register(n.condition.place)
        local true_label = operand.lb()
        local end_label = operand.lb()
        table.insert(tac[self.method.id], {type="cmp", first=n.place, second=operand.i(0)})
        table.insert(tac[self.method.id], {type="jne", target=true_label})
        if(n.false_case) then
            emit_statement(n.false_case)
        end
        table.insert(tac[self.method.id], {type="jmp", target=end_label})
        table.insert(tac[self.method.id], {type="label", target=true_label})
        emit_statement(n.true_case)
        table.insert(tac[self.method.id], {type="label", target=end_label})

    end

    function emit_for(n)
        local start_label = operand.lb()
        local update_label = operand.lb()
        local end_label = operand.lb()
        table.insert(self.loop_labels, {update_lb=update_label, end_lb=end_label})
        if(node_check(n.initialization, "DECLARATION")) then
            emit_declaration(n.initialization)
        else
            emit_expression(n.initialization)
        end
        table.insert(tac[self.method.id], {type="label", target=start_label})
        emit_expression(n.condition)
        n.place = load_operand_into_register(n.condition.place)
        table.insert(tac[self.method.id], {type="cmp", first=n.place, second=operand.i(0)})
        table.insert(tac[self.method.id], {type="je", target=end_label})
        emit_statement(n.statement)
        table.insert(tac[self.method.id], {type="label", target=update_label})
        emit_expression(n.update)
        table.insert(tac[self.method.id], {type="jmp", target=start_label})
        table.insert(tac[self.method.id], {type="label", target=end_label})
        table.remove(self.loop_labels)
    end

    function emit_while(n)
        local start_label = operand.lb()
        local end_label = operand.lb()
        table.insert(self.loop_labels, {update_lb=start_label, end_lb=end_label})
        table.insert(tac[self.method.id], {type="label", target=start_label})
        emit_expression(n.condition)
        n.place = load_operand_into_register(n.condition.place)
        table.insert(tac[self.method.id], {type="cmp", first=n.place, second=operand.i(0)})
        table.insert(tac[self.method.id], {type="je", target=end_label})
        emit_statement(n.statement)
        table.insert(tac[self.method.id], {type="jmp", target=start_label})
        table.insert(tac[self.method.id], {type="label", target=end_label})
        table.remove(self.loop_labels)
    end

    function emit_break(n)
        table.insert(tac[self.method.id], {type="jmp", target=self.loop_labels[#self.loop_labels].end_lb})
    end

    function emit_continue(n)
        table.insert(tac[self.method.id], {type="jmp", target=self.loop_labels[#self.loop_labels].update_lb})
    end

    function emit_return(n)
        emit_expression(n.value)
        -- EDIT THIS
        if(memory_operands[n.value.place.type] or n.value.place.type == "i") then
            n.value.place = load_operand_into_register(n.value.place)
        end

        table.insert(tac[self.method.id], {type="mov", source=n.value.place, dest=self.RETURN_REG})
        table.insert(tac[self.method.id], {type="jmp", target=operand.i(".exit_"..self.method.id)})

    end

    -- function handle_name_definition_conflict(id)
    --     -- generally a name can only be redefined or shadowed if it exists in a scope level less than the current scope level
    --     if(current_scope[id]) then
    --         error(string.format("Symbol '%s' has already been defined in scope level %d", id, current_scope.level))
    --     end
    -- end

    function emit_function_call(n)
        if(n.id == "print_num" or n.id == "printf") then
            emit_assignment_expression(n.args[1])
            if(memory_operands[n.args[1].place.type]) then
                table.insert(tac[self.method.id], {type="ld", source=n.args[1].place, dest=operand.r("r1")})
            else
                table.insert(tac[self.method.id], {type="mov", source=n.args[1].place, dest=operand.r("r1")})
            end
            table.insert(tac[self.method.id], {type="call", target=n.id})
        else
            emit_argument_list(n.args)
            table.insert(tac[self.method.id], {type="call", target=n.id})
            table.insert(tac[self.method.id], {type="add", source=operand.i(#n.args), dest=self.STACK_POINTER}) -- destroy stack frame
        end
    end

    function emit_argument_list(n, is_standard_function)
        for i=#n, 1, -1 do
            local a = n[i]
            emit_assignment_expression(a)
            if(not is_standard_function) then
                if(not register_operands[a.place.type] or a.place.type == "i") then
                    a.place = load_operand_into_register(a.place)
                end
                table.insert(tac[self.method.id], {type="push", target=a.place})
        
            else
                emit_move(a.place, self.standard_function_arguments[#self.standard_function_arguments - i + 1])
            end
        end
    end
        

    function emit_sum_expression(n)
        if(not node_check(n, "SUM_EXPRESSION")) then
            emit_term(n)
            return
        end
    
        emit_term(n[1])
        n.place = load_operand_into_register(n[1].place)
        for i = 3, #n, 2 do
            
            emit_term(n[i])
            -- term
            if(memory_operands[n[i].place.type]) then
                n[i].place = load_operand_into_register(n[i].place)
            end

            if(n[i-1].type == TOKEN_TYPES['+']) then
                if(n.value_type.kind == Type.KINDS["POINTER"] or n.value_type.kind == Type.KINDS["ARRAY"]) then
                    emit_abstract_add(n[i].place, n.place, self:sizeof(n.value_type.points_to))
                else
                    table.insert(tac[self.method.id], {type="add", source = n[i].place, dest=n.place})
                end
            else
                table.insert(tac[self.method.id], {type="sub", source = n[i].place, dest=n.place})
            end
        end
    end

    function emit_abstract_add(source, dest, size)
        -- source is either an imm or reg while dest is a reg
        if(source.type == "i") then
            if(source.value == 0) then
                return
            end
        elseif(not register_operands[source.type]) then
            source = load_operand_into_register(source)
        end

        if(size > 1) then
            if(source.type == "i") then
                source.value = size * source.value
            else
                table.insert(tac[self.method.id], {type="mull", source=operand.i(size), dest=source})
            end
        end
        table.insert(tac[self.method.id], {type="add", source=source, dest=dest})
    end

    function emit_term(n)
        if(not node_check(n, "MULTIPLICATIVE_EXPRESSION")) then
            emit_cast_expression(n)
            return
        end
        emit_cast_expression(n[1])
        if(#n > 1) then
            n.place = load_operand_into_register(n[1].place)
        else
            n.place = n[1].place
        end
        for i = 3, #n, 2 do
            
            emit_cast_expression(n[i])
            -- factor
            if(memory_operands[n[i].place.type]) then
                n[i].place = load_operand_into_register(n[i].place)
            end

            if(n[i-1].type == TOKEN_TYPES['*']) then
                table.insert(tac[self.method.id], {type="mull", source = n[i].place, dest=n.place})
            else
                table.insert(tac[self.method.id], {type="div", source = n[i].place, dest=n.place})
            end
        end
    end

    function emit_cast_expression(n)
        
        if(node_check(n, "CAST_EXPRESSION")) then
            emit_cast_expression(n.cast_expression)
            n.place = n.cast_expression.place
        else
            emit_unary_expression(n)
        end
    end

    function emit_unary_expression(n)
        if(node_check(n, "UNARY_EXPRESSION")) then
            if(n.operator == "++") then
                emit_unary_expression(n.child)
                local next_reg = load_operand_into_register(n.child.place)
                emit_abstract_add(operand.i(1),next_reg, self:sizeof(n.child.value_type.points_to))
                emit_move(next_reg, n.child.place)
                n.place = next_reg
            elseif(n.operator == "--") then
                emit_unary_expression(n.child)
                local next_reg = load_operand_into_register(n.child.place)
                table.insert(tac[self.method.id], {type="sub", source=operand.i(1), dest=next_reg})
                emit_move(next_reg, n.child.place)
                n.place = next_reg
            elseif(n.operator == "SIZEOF") then
                emit_cast_expression(n.child)
                n.place = operand.i(self:sizeof(n.child.value_type))
                table.insert(tac[self.method.id], {type="mov"})
                error()
            elseif(n.operator == "&") then
                emit_cast_expression(n.child)
                if(n.child.place.type == "pr") then
                    n.place = n.child.place
                    n.place.type = "t" -- ok to clobber child's place
                else
                    n.place = operand.t()
                    table.insert(tac[self.method.id], {type="!get_address", target=n.child.place, dest=n.place})
                end
            elseif(n.operator == "*") then
                emit_cast_expression(n.child)
                n.place = emit_dereference(n.child.place)
            elseif(n.operator == "-") then
                emit_cast_expression(n.child)
                n.place = load_operand_into_register(n.child.place)
                table.insert(tac[self.method.id], {type="xor", source=operand.i(65535), dest=n.place}) -- take the two's complement
                table.insert(tac[self.method.id], {type="add", source=operand.i(1), dest=n.place})
            elseif(n.operator == "~") then
                emit_cast_expression(n.child)
                n.place = load_operand_into_register(n.child.place)
                table.insert(tac[self.method.id], {type="xor", source=operand.i(65535), dest=n.place})
            elseif(n.operator == "+") then
                emit_cast_expression(n.child)
                n.place = n.child.place
            elseif(n.operator == "!") then
                emit_cast_expression(n.child)
                n.place = load_operand_into_register(n.child.place)
                local true_label = operand.lb()
                local end_label = operand.lb()
                table.insert(tac[self.method.id], {type="cmp", first=n.place, second=operand.i(0)})
                table.insert(tac[self.method.id], {type="jne", target=true_label})
                emit_move(operand.i(0), n.place)
                table.insert(tac[self.method.id], {type="jmp", target=end_label})
                table.insert(tac[self.method.id], {type="label", target=true_label})
                emit_move(operand.i(1), n.place)
                table.insert(tac[self.method.id], {type="label", target=end_label})
            else
                error()
            end
        else
            emit_postfix_expression(n)
        end
    end

    function emit_dereference(place)
        local pr = nil
        if(place.type == "g" or place.type == "p" or place.type == "l") then
            pr = operand.pr()
            table.insert(tac[self.method.id], {type="ld", source=place, dest=pr})
        elseif(place.type == "t") then
            -- treat as pr
            place.type = "pr"
            pr = place
        elseif(place.type == "pr") then
            pr = place
        else
            error()
        end
        return pr
    end

    function emit_postfix_expression(n)
        if(node_check(n, "POSTFIX_EXPRESSION")) then
            emit_primary_expression(n.primary_expression)
            n.place = n.primary_expression.place
            -- if(n.primary_expression.place.type ~= "pr" and n.place.type ~= "i") then
            --     n.place = load_operand_into_register(n.place)
            -- end
            for i, operation in ipairs(n) do
                if(operation.type == "[") then
                    emit_expression(operation.value)
                    if(not register_operands[n.place.type] and n.place.type ~= "pr") then
                        n.place = load_operand_into_register(n.place)
                    end
                    emit_abstract_add(operation.value.place, n.place, self:sizeof(n.value_types[i]))
                    n.place = emit_dereference(n.place)
                elseif(operation.type == "(") then
                    emit_argument_list(operation.value, n.place.is_standard_function)

                    if(memory_operands[n.place.type]) then
                        n.place = load_operand_into_register(n.place)
                    end
                    table.insert(tac[self.method.id], {type="call", target=n.place}) -- fix this
                    if(not n.place.is_standard_function) then
                        table.insert(tac[self.method.id], {type="add", source=operand.i(#operation.value), dest=self.STACK_POINTER}) -- destroy stack frame
                    end
                    if(n.value_types[i].kind ~= Type.KINDS["VOID"]) then
                        n.place = operand.t()
                        emit_move(self.RETURN_REG, n.place)
                    end
                elseif(operation.type == "++") then
                    local next_reg = load_operand_into_register(n.place)
                    local temp_reg = operand.t()
                    emit_move(next_reg, temp_reg)
                    emit_abstract_add(operand.i(1), next_reg, self:sizeof(n.value_types[i].points_to))
                    emit_move(next_reg, n.place)
                    n.place = temp_reg
                elseif(operation.type == ".") then
                    local member_type = n.value_types[i-1].members[operation.value.id].type
                    if(n.place.type ~= "pr") then
                        local next_reg = operand.pr()
                        table.insert(tac[self.method.id], {type="!get_address", target=n.place, dest=next_reg})
                        n.place = next_reg
                    end
                    if(n.value_types[i-1].members[operation.value.id].offset > 0) then
                        table.insert(tac[self.method.id], {type="add", source=operand.i(n.value_types[i-1].members[operation.value.id].offset), dest=n.place})
                    end
                elseif(operation.type == "->") then
                    if(not register_operands[n.place.type] and n.place.type ~= "pr") then
                        local next_reg = operand.pr() -- ? can this be a pr?
                        table.insert(tac[self.method.id], {type="!get_address", target=n.place, dest=next_reg})
                        n.place = next_reg
                    end
                    
                    table.insert(tac[self.method.id], {type="ld", source=n.place, dest=n.place})
                    local offset = n.value_types[i-1].points_to.members[operation.value.id].offset
                    if(offset > 0) then
                        table.insert(tac[self.method.id], {type="add", source=operand.i(offset), dest=n.place})
                    end
                end
            end
        else
            emit_primary_expression(n)
        end
    end

    

    function emit_primary_expression(n)
        
        if(node_check(n, "INT")) then
            n.place = operand.i(n.value)
        elseif(node_check(n, "IDENTIFIER")) then
            local symbol = n.handle
            if(symbol) then
                if(symbol.type.kind == Type.KINDS["ARRAY"]) then
                    n.place = operand.pr()
                    table.insert(tac[self.method.id], {type="!get_address", target=symbol.place, dest=n.place})
                else
                    n.place = symbol.place
                end
            else
                error(string.format("Variable '%s' used before definition", n.value))
            end
        elseif(node_check(n, "CHARACTER")) then
            n.place = operand.i(n.value)
        elseif(node_check(n, "EXPRESSION")) then
            emit_expression(n);
        elseif(node_check(n, "STRING_LITERAL")) then
            local temp = operand.g(n.value_type.length)
            register_string_literal(n, temp)
            n.place = operand.t()
            table.insert(tac[self.method.id], {type="!get_address", target=temp, dest=n.place}) -- some r value registers can hold an address; however, the address cannot be used to store data
        else
            print(Node.INVERTED_NODE_TYPES[n.type])
            error()
        end
    end

    emit_program(ast)

    return self
end

return IRVisitor