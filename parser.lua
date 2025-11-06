local Token = require('token')
local Node = require('node')

local Parser = {}

function Parser.parse(toks)
    local TOKEN_TYPES = Token.TOKEN_TYPES
    local INVERTED_TOKENS = Token.INVERTED_TOKENS
    local NODE_TYPES = Node.NODE_TYPES

    function new(type)
        return Node:new(NODE_TYPES[type])
    end
    --local symbol_table = {}
    local ast = {}
    
    local i = 0
    function next_token()
        i = i + 1
        return toks[i]
    end

    function peek_token()
        -- The very last token is guaranteed to be EOF
        return toks[i + 1]
    end

    function expect(type)
        local t = next_token()
        if not t or t.type ~= type then
            error("Expected '" .. INVERTED_TOKENS[type] .. "', Received '" .. (t and t.value or "EOF") .. "'")
        end
    end

    function accept(type)
        local t = peek_token()
        if t and t.type == type then
            next_token()
            return true
        else
            return false
        end
    end

    function check(type)
        local t = peek_token()
        return t and t.type == type
    end            

    function parse_program()
        local program_node = Node:new(NODE_TYPES["Program"])
        
        while peek_token().type ~= TOKEN_TYPES["EOF"] do

            table.insert(program_node, parse_declaration())
        end

        return program_node
    end

    function parse_declaration()
        local declaration_node = Node:new(NODE_TYPES["Declaration"])

        declaration_node.type_specifier = parse_type_specifier()

        declaration_node.id = parse_identifier()

        local token = next_token()

        if token.type == TOKEN_TYPES["("] then
            declaration_node.parameter_list = parse_parameter_list()
            declaration_node.id.is_function = true;

            expect(TOKEN_TYPES[")"])

            declaration_node.block = parse_block()
        elseif token.type == TOKEN_TYPES["="] then
            declaration_node.value = parse_expression()
            declaration_node.id.is_function = false;
            expect(TOKEN_TYPES[";"])
        else
            error("Unexpected token: " .. token.value)
        end

        return declaration_node
    end

    function parse_parameter_list()
        local parameter_list_node = Node:new(NODE_TYPES["Parameter_List"])
        if peek_token().type == TOKEN_TYPES[")"] then
            return parameter_list_node
        end

        table.insert(parameter_list_node, parse_parameter())
        while peek_token().type == TOKEN_TYPES[","] do
            next_token()
            table.insert(parameter_list_node, parse_parameter())
        end

        return parameter_list_node
    end

    function parse_parameter()
        local parameter_node = Node:new(NODE_TYPES["PARAMETER"])
        parameter_node.type_specifier = parse_type_specifier()
        parameter_node.id = parse_identifier()

        return parameter_node
    end

    function parse_block()
        local block_node = Node:new(NODE_TYPES["Block"])

        if next_token().type == TOKEN_TYPES["{"] then
            while peek_token().type ~= TOKEN_TYPES["}"] do
                table.insert(block_node, parse_statement())
                expect(TOKEN_TYPES[";"])
            end
            next_token()
        else
            error("Expected '{' to start block")
        end

        return block_node
    end

    function parse_statement()
        local statement_node = Node:new(NODE_TYPES["Statement"])
        local token = peek_token()
        -- Declaration
        if token.type == TOKEN_TYPES["TYPE_SPECIFIER"] then
            statement_node.child = parse_local_declaration()
        elseif check(TOKEN_TYPES["ID"]) or check(TOKEN_TYPES["*"]) then
            if(toks[i+2] and (toks[i+2].type == TOKEN_TYPES["("])) then
                statement_node.child = parse_function_call()
            else
                statement_node.child = parse_assignment()
            end
        elseif token.type == TOKEN_TYPES["IF"] then
            statement_node.child = parse_if()
        elseif token.type == TOKEN_TYPES["FOR"] then
            statement_node.child = parse_for()
        elseif token.type == TOKEN_TYPES["WHILE"] then
            statement_node.child = parse_while()
        elseif token.type == TOKEN_TYPES["RETURN"] then
            statement_node.child = parse_return()
        else
            error("Unexpected token: " .. token.value)
        end

        return statement_node
    end

    function parse_assignment()
        local assignment_node = Node:new(NODE_TYPES["Assignment"])
        if check(TOKEN_TYPES["*"]) then
            assignment_node.indirection_level = 0
            while accept(TOKEN_TYPES["*"]) do
                assignment_node.indirection_level = assignment_node.indirection_level + 1
            end
        end

        assignment_node.id = parse_identifier()

        if next_token().type == TOKEN_TYPES["="] then
            assignment_node.value = parse_expression()
        else
            error("Unexpected token " .. INVERTED_TOKENS[peek_token().type])
        end


        return assignment_node;
    end

    function parse_return()
        local return_node = Node:new(NODE_TYPES["RETURN"])
        next_token()
        return_node.value = parse_expression()

        return return_node
    end


    function parse_local_declaration()
        local declaration_node = Node:new(NODE_TYPES["Local_Declaration"])

        declaration_node.type_specifier = parse_type_specifier()

        declaration_node.id = parse_identifier()

        if next_token().type == TOKEN_TYPES["="] then
            declaration_node.value = parse_expression()
        end

        return declaration_node
    end

    function parse_type_specifier()
        local type_specifier_node = Node:new(NODE_TYPES["Type_Specifier"])

        local token = next_token()

        if token.value == "int" or token.value == "char" or token.value == "void" then
            type_specifier_node.value = token.value
        else
            error("Unexpected type specifier: " .. token.value)
        end

        type_specifier_node.indirection_level = 0;

        while accept(TOKEN_TYPES["*"]) do
            type_specifier_node.indirection_level = type_specifier_node.indirection_level + 1
        end

        return type_specifier_node
    end

    function parse_identifier()
        local identifier_node = Node:new(NODE_TYPES["Identifier"])

        local token = next_token()

        if token.type == TOKEN_TYPES["ID"] then
            identifier_node.id = token.value
        else
            error("Unexpected identifier: " .. token.value)
        end

        return identifier_node
    end

    function parse_expression()
        local expression_node = Node:new(NODE_TYPES["Expression"])


        table.insert(expression_node, parse_term())

        local operator = peek_token()
        while operator.type == TOKEN_TYPES["+"] or operator.type == TOKEN_TYPES["-"] do
            next_token()
            table.insert(expression_node, operator)
            table.insert(expression_node, parse_term())
            operator = peek_token()
        end

        return expression_node
    end

    function parse_term()
        local term_node = Node:new(NODE_TYPES["Term"])

        table.insert(term_node, parse_factor())

        local operator = peek_token()
        while operator.type == TOKEN_TYPES["*"] or operator.type == TOKEN_TYPES["/"] or operator.type == TOKEN_TYPES["%"] do
            next_token()
            table.insert(term_node, operator)
            table.insert(term_node, parse_factor())
            operator = peek_token()
        end

        return term_node
    end

    function parse_factor()
        local factor_node = new("FACTOR")

        local token = peek_token()
        if token.type == TOKEN_TYPES["INT"] then
            next_token()
            local int_node = Node:new(NODE_TYPES["Int"])
            int_node.value = token.value

            factor_node.value = int_node
        elseif token.type == TOKEN_TYPES["ID"] then
            if(toks[i + 2].type == TOKEN_TYPES["("]) then
                factor_node.value = parse_function_call()
            else
                next_token()
                local identifier_node = Node:new(NODE_TYPES["Identifier"])
                identifier_node.id = token.value

                factor_node.value = identifier_node
            end
        elseif token.type == TOKEN_TYPES["("] then
            next_token()
            factor_node.value = parse_expression()

            expect(TOKEN_TYPES[")"])
        elseif check(TOKEN_TYPES["&"]) then
            factor_node.value = parse_address_of()
        elseif check(TOKEN_TYPES["*"]) then
            factor_node.value = parse_dereference()
        else
            error("Unexpected token: " .. token.value)
        end

        return factor_node
    end

    function parse_dereference()
        local dereference_node = new("DEREFERENCE")
        expect(TOKEN_TYPES["*"])
        if check(TOKEN_TYPES["ID"]) then
 
            dereference_node.id = next_token().value
        else
            error("Can only dereference a variable")
        end

        return dereference_node
    end

    function parse_address_of()
        local address_of_node = new("ADDRESS_OF")
        expect(TOKEN_TYPES["&"])
        
        if check(TOKEN_TYPES["ID"]) then
            address_of_node.id = next_token().value
        else
            error("Can only perform an address of operation on a variable")
        end

        return address_of_node
    end




    function parse_function_call()
        local function_call_node = Node:new(NODE_TYPES["FUNCTION_CALL"])
        function_call_node.id = next_token().value
        next_token()
        function_call_node.args = parse_argument_list()
        next_token()

        return function_call_node
    end

    function parse_argument_list()
        local argument_list_node = Node:new(NODE_TYPES["ARGUMENT_LIST"])
        if peek_token().type == TOKEN_TYPES[")"] then
            return argument_list_node
        end

        table.insert(argument_list_node, parse_expression())

        while peek_token().type == TOKEN_TYPES[","] do
            next_token()
            table.insert(argument_list_node, parse_expression())
        end

        return argument_list_node

    end

    function parse_if()
        local if_node = Node:new(NODE_TYPES["If"])
        next_token()

        if next_token().type ~= TOKEN_TYPES["("] then
            error("Expected '(' after 'if'")
        end

        if_node.condition = parse_expression()
        if next_token().type ~= TOKEN_TYPES[")"] then
            error("Expected ')' after condition")
        end

        if_node.block = parse_block()

        return if_node
    end

    function parse_for()
        local for_node = Node:new(NODE_TYPES["For"])

        next_token()
        if next_token().type ~= TOKEN_TYPES["("] then
            error("Expected '(' after 'for'")
        end

        for_node.initialization = parse_expression()

        if next_token().type ~= TOKEN_TYPES[";"] then
            error("Expected ';' after initialization")
        end

        for_node.condition = parse_expression()

        if next_token().type ~= TOKEN_TYPES[";"] then
            error("Expected ';' after condition")
        end

        for_node.update = parse_expression()

        if next_token().type ~= TOKEN_TYPES[")"] then
            error("Expected ')' after update")
        end

        for_node.block = parse_block()

        return for_node
    end

    return parse_program()
end



return Parser


