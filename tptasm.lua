if rawget(_G, "setfenv") then
	setfenv(1, { _G = _G, modules = {} })
else
	_ENV = { _G = _G, modules = {} }
end

modules["tptasm"] = function()
local modulepack = require("modulepack")
local printf     = require("tptasm.printf")
local utility    = require("tptasm.utility")

local function main(...)
	local exit_with = 0

	printf.init()

	local args = { ... }
	modulepack.xpcall_wrap(function()

		local detect = require("tptasm.detect")
		local archs = require("tptasm.archs")
		local preprocess = require("tptasm.preprocess")
		local emit = require("tptasm.emit")
		local resolve = require("tptasm.resolve")
		local xbit32 = require("tptasm.xbit32")

		local named_args, unnamed_args = utility.parse_args(args)

		local log_path = named_args.log or unnamed_args[3]
		if log_path then
			printf.redirect(log_path)
		end

		if type(named_args.anchor) == "string" then
			detect.make_anchor(named_args.anchor, named_args.anchor_dx, named_args.anchor_dy, named_args.anchor_prop, named_args.anchor_id)
			return
		end

		if named_args.silent then
			printf.silent = true
		end

		if named_args.detect then
			printf.info("listing targets")
			local counter = 0
			for x, y, model, id in detect.all_cpus() do
				printf.info(" * %s with ID %i at (%i, %i)", model, id, x, y)
				counter = counter + 1
			end
			printf.info("found %s targets", counter)
			return
		end

		local target = named_args.target or unnamed_args[2]
		local model_name = named_args.model or unnamed_args[4]

		if not model_name then
			model_name = detect.model(type(target) == "number" and target)
		end
		if not model_name then
			printf.failf("failed to detect model and no model name was passed")
		end

		local architecture_name = archs.get_name(model_name) or printf.failf("no architecture description for model '%s'", model_name)
		local architecture = archs.get_description(architecture_name)

		local root_source_path = tostring(named_args.source or unnamed_args[1] or printf.failf("no source specified"))
		local lines = preprocess(architecture, root_source_path)
		local to_emit, labels, model_restrictions = resolve.instructions(architecture, lines)

		for path, restrictions in pairs(model_restrictions) do
			local path_ok = false
			local failing_restriction_tokens = {}
			for _, restriction in ipairs(restrictions) do
				if model_name:find("^" .. restriction.value:gsub("^\"(.*)\"$", "%1") .. "$") then
					path_ok = true
				else
					failing_restriction_tokens[restriction] = true
				end
			end
			if not path_ok then
				local report_with = named_args.allow_model_mismatch and printf.warn or printf.err
				report_with("%s: unit incompatible with target model", path)
				for token in pairs(failing_restriction_tokens) do
					token:blamef(printf.info, "target model doesn't match this pattern")
				end
			end
		end
		if printf.err_called then
			printf.failf("model restriction check failed, bailing")
		end

		if named_args.export_labels then
			local handle = io.open(named_args.export_labels, "w")
			if handle then
				local sorted_labels = {}
				for name, address in pairs(labels) do
					table.insert(sorted_labels, {
						name = name,
						address = tonumber(address)
					})
				end
				table.sort(sorted_labels, function(lhs, rhs)
					if lhs.address < rhs.address then return  true end
					if lhs.address > rhs.address then return false end
					if lhs.name    < rhs.name    then return  true end
					if lhs.name    > rhs.name    then return false end
					return false
				end)
				for _, label in ipairs(sorted_labels) do
					handle:write(("%s 0x%X\n"):format(label.name, label.address))
				end
				handle:close()
			else
				printf.warn("failed to open '%s' for writing, no labels exported", tostring(named_args.export_labels))
			end
		end
		local opcodes = emit(architecture, to_emit, labels)

		if type(target) == "table" then
			for ix, ix_opcode in pairs(opcodes) do
				target[ix] = ix_opcode
			end
		elseif type(target) == "string" then
			local buf = {}
			local _, first = next(opcodes)
			if first then
				local width = #first.dwords
				for ix, ix_opcode in pairs(opcodes) do
					for ix_dword = 1, width do
						local dword = ix_opcode.dwords[ix_dword]
						buf[ix * width + ix_dword] = string.char(
							xbit32.band(              dword     , 0xFF),
							xbit32.band(xbit32.rshift(dword,  8), 0xFF),
							xbit32.band(xbit32.rshift(dword, 16), 0xFF),
							xbit32.band(xbit32.rshift(dword, 24), 0xFF)
						)
					end
				end
			end
			local handle = io.open(target, "wb")
			if handle then
				handle:write(table.concat(buf))
				handle:close()
			else
				printf.warn("failed to open '%s' for writing, no opcodes written", target)
			end
		else
			architecture.flash(model_name, target, opcodes)
			if printf.err_called then
				printf.failf("flashing stage failed, bailing")
			end
		end

	end, function(err)

		if err == printf.failf then
			exit_with = 1
		else
			-- * Dang.
			printf.info("this is an assembler bug, tell LBPHacker!")
			printf.info("https://github.com/LBPHacker/tptasm")
			exit_with = 2
		end

	end)()

	printf.unredirect()
	printf.info("done")
	if not _G.tpt then
		os.exit(exit_with)
	end
	return exit_with
end

local function run(...)
	if _G.tpt and select("#", ...) == 0 then
		_G.tptasm = modulepack.xpcall_wrap(main)
		return
	end
	return main(...)
end

return {
	run = run,
}

end

modules["tptasm.archs"] = function()
local known_archs = {
	[   "A728D28" ] = require("tptasm.archs.a728d28"  ),
	[ "B29K1QS60" ] = require("tptasm.archs.b29k1qs60"),
	[  "I8M7D28S" ] = require("tptasm.archs.i8m7d28s" ),
	[      "MAPS" ] = require("tptasm.archs.maps"     ),
	[   "MICRO21" ] = require("tptasm.archs.micro21"  ),
	[      "PTP7" ] = require("tptasm.archs.ptp7"     ),
	[        "R2" ] = require("tptasm.archs.r2"       ),
	[        "R3" ] = require("tptasm.archs.r3"       ),
	[ "Armatoste" ] = require("tptasm.archs.armatoste"),
}
local function get_description(architecture_name)
	return known_archs[architecture_name]
end

local known_models_to_archs = {
	[  "A728D280" ] = "A728D28",   -- * "A7-28D28 Microcomputer" by Sam_Hayzen, id:2460726
	[  "A728D28A" ] = "A728D28",   -- * "A7-28D28 Microcomputer" by Sam_Hayzen, id:2460726
	[ "B29K1QS60" ] = "B29K1QS60", -- * "B29K1QS60" by unnick, id:2435570
	[  "I8M7D28S" ] = "I8M7D28S",  -- * "Guardian I8M7D28S" by Sam_Hayzen, id:2473628
	[      "MAPS" ] = "MAPS",      -- * "Computer (mapS)" by drakide, id:975033
	[   "MICRO21" ] = "MICRO21",   -- * "Micro Computer v2.1" by RockerM4NHUN, id:1599945
	[     "PTP7A" ] = "PTP7",      -- * "PTP7" by unnick, id:2458644
	[   "R216K2A" ] = "R2",        -- * "R216K2A" by LBPHacker, id:2303519
	[   "R216K4A" ] = "R2",        -- * "R216K4A" by LBPHacker, id:2305835
	[   "R216K8B" ] = "R2",        -- * "R216K8B" by LBPHacker, id:2342633
	[ "Armatoste" ] = "Armatoste", -- * yet unreleased architecture by DanielUbTb
}
for core_count = 1, 99 do
	for memory_rows = 1, 64 do
		known_models_to_archs[("R3A%02i%02i"):format(memory_rows, core_count)] = "R3" -- * "R3A1016" et al by LBPHacker, id:3236906
	end
end

local function get_name(model_name)
	return known_models_to_archs[model_name]
end

return {
	get_description = get_description,
	get_name = get_name,
}

end

modules["tptasm.archs.a728d28"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local detect = require("tptasm.detect")
local xbit32 = require("tptasm.xbit32")
local hacks  = require("tptasm.archs.hacks")

local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_

		%define dw `dw'
		%define org `org'

		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap]
	end)
}

local entities = {
	["r0"] = { type = "register", offset = 0 },
	["r1"] = { type = "register", offset = 1 },
	["r2"] = { type = "register", offset = 2 },
	["r3"] = { type = "register", offset = 3 },
}

local mnemonics = {
	[  "ldi"] = { class = "1I", bit =  0, flags = " s n " }, -- * r: write to and lock RP
	[   "ld"] = { class = "1R", bit =  3, flags = "r    " }, -- * s: redirect explicit branch ptr to P
	[   "st"] = { class = "1R", bit =  4, flags = "r    " }, -- * b: write to and lock EBP^
	[  "stl"] = { class = "0" , bit =  5, flags = "     " }, -- * n: write to and lock NIP
	["andrl"] = { class = "0" , bit =  6, flags = "     " }, -- * p: write to and lock P
	[  "xor"] = { class = "0" , bit =  7, flags = "     " },
	[   "rr"] = { class = "0" , bit =  8, flags = "     " },
	[ "setc"] = { class = "0" , bit =  9, flags = "     " },
	[  "brc"] = { class = "1I", bit = 10, flags = " s n " },
	[ "brnc"] = { class = "1I", bit = 10, flags = "    p" },
	[  "out"] = { class = "0" , bit = 11, flags = "     " },
	[ "bsto"] = { class = "0" , bit = 12, flags = "     " },
	[  "ldb"] = { class = "0" , bit = 13, flags = " s   " },
	[   "br"] = { class = "1I", bit = -1, flags = "  b  " },
}

local nop = opcode.make(28)

local dw_bits = 29

local mnemonic_desc = {}

function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token_hax, parameters_hax, offset)
	local sub_instructions = hacks.pig(mnemonic_token_hax, parameters_hax, mnemonics)
	if not sub_instructions then
		return false
	end

	local final_code = nop:clone()
	local bit_invoked_by = {}
	local rp_locked = false
	local rp_value = 0
	local nip_locked = false
	local nip_value = 0
	local p_locked = false
	local p_value = 0
	local explicit_branch = false
	local branch_to_p = false
	for _, subinst in ipairs(sub_instructions) do
		if subinst.instr_desc.flags:find("s") then
			branch_to_p = true
		end
	end
	for _, subinst in ipairs(sub_instructions) do
		local bit = subinst.instr_desc.bit
		if bit_invoked_by[bit] then
			subinst.mnemonic_token:blamef(printf.err, "micro-instruction invoked multiple times")
			bit_invoked_by[bit]:blamef(printf.info, "first invoked here")
			return false
		end
		bit_invoked_by[bit] = subinst.mnemonic_token

		local imm
		if subinst.operand and subinst.operand:number() then
			imm = subinst.operand.parsed
			local trunc = 0x80
			if imm >= trunc then
				imm = imm % trunc
				subinst.operand:blamef(printf.warn, "number truncated to 7 bits")
			end
		end

		local temp_flags = subinst.instr_desc.flags
		if temp_flags:find("b") then
			explicit_branch = true
			if branch_to_p then
				temp_flags = temp_flags .. "p"
			else
				temp_flags = temp_flags .. "n"
			end
		end

		if temp_flags:find("p") then
			if p_locked and subinst.operand.parsed ~= p_value then
				subinst.mnemonic_token:blamef(printf.err, "general pointer already locked")
				p_locked:blamef(printf.info, "locked by this")
				return false
			end
			p_locked = subinst.mnemonic_token
			p_value = imm
		end

		if temp_flags:find("n") then
			if nip_locked and subinst.operand.parsed ~= nip_value then
				subinst.mnemonic_token:blamef(printf.err, "next instruction pointer already locked")
				nip_locked:blamef(printf.info, "locked by this")
				return false
			end
			nip_locked = subinst.mnemonic_token
			nip_value = imm
		end

		if temp_flags:find("r") then
			if rp_locked and subinst.operand.entity.offset ~= rp_value then
				subinst.mnemonic_token:blamef(printf.err, "register bits already locked")
				rp_locked:blamef(printf.info, "locked by this")
				return false
			end
			rp_locked = subinst.mnemonic_token
			rp_value = subinst.operand.entity.offset
		end

		final_code:merge(1, bit)
	end
	if not explicit_branch then
		local next_inst = xbit32.band(offset + 1, 0x7F)
		if branch_to_p then
			p_value = next_inst
		else
			nip_value = next_inst
		end
	end
	final_code:merge(p_value, 21)
	final_code:merge(nip_value, 14)
	final_code:merge(rp_value, 1)

	return true, { final_code }
end

local mnemonics = {}
for key in pairs(mnemonics) do
	mnemonics[key] = mnemonic_desc
end

local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end

	local space_available = ({
		["A728D280"] =  0,
		["A728D28A"] = 16,
	})[model]
	if #opcodes >= space_available then
		printf.err("out of space; code takes %i cells, only have %i", #opcodes + 1, space_available)
		return
	end

	for ix = 0, #opcodes do
		sim.partProperty(sim.partID(x + 18 - ix, y - 9), "ctype", 0x20000000 + opcodes[ix].dwords[1])
	end
end

return {
	includes = includes,
	dw_bits = dw_bits,
	nop = nop,
	entities = entities,
	mnemonics = mnemonics,
	flash = flash,
}

end

modules["tptasm.archs.armatoste"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local detect = require("tptasm.detect")

local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_
		
		%define dw `dw'
		%define org `org'

		%macro mov OutRegis, InRegOrImm
			orr OutRegis, r0, InRegOrImm
		%endmacro

		%macro not OutRegis, In
			sub te, r0, 1
			xor outRegis, te, In
		%endmacro

		%macro nop
			orr r0, r0, r0
		%endmacro

		%macro jum Address
			jal r0, Address
		%endmacro

		%macro bls In1, In2, Address
			sls	te, In1, In2
			bne	te, r0, Address
		%endmacro

		%macro bge In1, In2, Address
			sls	te, In1, In2
			beq	te, r0, Address
		%endmacro

		%macro bgr In1, In2, Address
			sls	te, In2, In1
			bne	te, r0, Address
		%endmacro

		%macro ble In1, In2, Address
			sls	te, In2, In1
			beq	te, r0, Address
		%endmacro

		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap]
	end)
}

local dw_bits = 29

local nop = opcode.make(32):merge(0x2C000000, 0)

local entities = {
	[  "r0" ] = { type = "register", offset =  0 },
	[  "r1" ] = { type = "register", offset =  1 },
	[  "r2" ] = { type = "register", offset =  2 },
	[  "r3" ] = { type = "register", offset =  3 },
	[  "r4" ] = { type = "register", offset =  4 },
	[  "r5" ] = { type = "register", offset =  5 },
	[  "r6" ] = { type = "register", offset =  6 },
	[  "r7" ] = { type = "register", offset =  7 },
	[  "r8" ] = { type = "register", offset =  8 },
	[  "r9" ] = { type = "register", offset =  9 },
	[ "r10" ] = { type = "register", offset = 10 },
	[ "r11" ] = { type = "register", offset = 11 },
	[ "r12" ] = { type = "register", offset = 12 },
	[ "r13" ] = { type = "register", offset = 13 },
	[ "r14" ] = { type = "register", offset = 14 },
	[ "r15" ] = { type = "register", offset = 15 },
}
entities["ra"] = entities["r10"]
entities["rb"] = entities["r11"]
entities["rc"] = entities["r12"]
entities["rd"] = entities["r13"]
entities["re"] = entities["r14"]
entities["rf"] = entities["r15"]
entities["ze"] = entities["r0"]
entities["te"] = entities["r13"]
entities["jl"] = entities["r14"]
entities["sp"] = entities["r15"]

local mnemonics = {}

local mnemonic_to_class_code = {
	[ "mju" ] = { class = " BX", code = 0x20000000 },
	[ "wmj" ] = { class = " BX", code = 0x20100000 },
	[ "jal" ] = { class = "A X", code = 0x21000000 },
	[ "beq" ] = { class = "ABX", code = 0x22000000 },
	[ "bne" ] = { class = "ABX", code = 0x23000000 },
	[ "loa" ] = { class = "ABX", code = 0x24000000 },
	[ "inn" ] = { class = "ABX", code = 0x25000000 },
	[ "sto" ] = { class = "ABX", code = 0x26000000 },
	[ "out" ] = { class = "ABX", code = 0x27000000 },
	[ "add" ] = { class = "ABX", code = 0x28000000 },
	[ "sub" ] = { class = "ABX", code = 0x29000000 },
	[ "lbl" ] = { class = "ABX", code = 0x2A000000 },
	[ "lbr" ] = { class = "ABX", code = 0x2B000000 },
	[ "and" ] = { class = "ABX", code = 0x2C000000 },
	[ "orr" ] = { class = "ABX", code = 0x2D000000 },
	[ "xor" ] = { class = "ABX", code = 0x2E000000 },
	[ "sls" ] = { class = "ABX", code = 0x2F000000 },
}

local mnemonic_desc = {}
function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token, parameters)
	local operands = {}
	for ix, ix_param in ipairs(parameters) do
		if #ix_param == 1
		   and ix_param[1]:is("entity") and ix_param[1].entity.type == "register" then
			table.insert(operands, {
				type = "reg",
				value = ix_param[1].entity.offset,
			})

		elseif #ix_param == 1
		   and ix_param[1]:number() then
			table.insert(operands, {
				type = "imm",
				value = ix_param[1].parsed,
				token = ix_param[1],
			})

		else
			if ix_param[1] then
				ix_param[1]:blamef(printf.err, "operand format not recognised")
			else
				ix_param.before:blamef_after(printf.err, "operand format not recognised")
			end
			return false

		end
	end

	local final_code
	local class_code = mnemonic_to_class_code[mnemonic_token.value]
	local code = opcode.make(32):merge(class_code.code, 0)
	local ok = true
	local function push(operand, shift)
		if operand.type == "reg" then
			code = code:merge(operand.value, shift)
		elseif shift == 0 and operand.type == "imm" then
			local width = 16
			local value = operand.value
			if value >= 2 ^ width then
				value = value % 2 ^ width
				operand.token:blamef(printf.warn, "number truncated to " .. width .. " bits")
			end
			code = code:merge(0x10000000, 0):merge(value, 0)
		else
			ok = false
		end
	end
	local index = 0
	if class_code.class:find("A") then
		index = index + 1
		push(operands[index], 20)
	end
	if class_code.class:find("B") then
		index = index + 1
		push(operands[index], 16)
	end
	if class_code.class:find("X") then
		index = index + 1
		push(operands[index],  0)
	end
	if index ~= #operands then
		ok = false
	end
	if ok then
		final_code = code
	end

	if not final_code then
		local operands_repr = {}
		for _, ix_oper in ipairs(operands) do
			table.insert(operands_repr, ix_oper.type)
		end
		mnemonic_token:blamef(printf.err, "no variant of %s exists that takes '%s' operands", mnemonic_token.value, table.concat(operands_repr, ", "))
		return false
	end

	return true, { final_code }
end

for mnemonic in pairs(mnemonic_to_class_code) do
	mnemonics[mnemonic] = mnemonic_desc
end

local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end

	local function filt(column, row)
		return x + 1 + column, y + 7 - row
	end

	local ram_width = 128
	local max_ram_height_order = 6
	local space_needed = #opcodes + 1
	local ram_height_order

	local vertical_space
	local seen_empty = false
	for row = 0, 2 ^ max_ram_height_order - 1 do
		local filled = true
		local empty = true
		for column = 0, ram_width - 1 do
			local id = sim.partID(filt(column, row))
			if id then
				empty = false
				if sim.partProperty(id, "type") ~= elem.DEFAULT_PT_FILT then
					filled = false
				end
			end
		end
		if filled and not seen_empty or empty then
			vertical_space = row + 1
		else
			break
		end
		if empty then
			seen_empty = true
		end
	end

	for order = 0, max_ram_height_order do
		if space_needed <= ram_width * 2 ^ order then
			if vertical_space < 2 ^ order then
				printf.err("out of vertical space; code takes %i cells, planned to build %i rows of RAM, could only build %i", space_needed, 2 ^ order, vertical_space)
				return
			end
			ram_height_order = order
			break
		end
	end
	if not ram_height_order then
		local space_available = ram_width * 2 ^ max_ram_height_order
		printf.err("out of space; code takes %i cells, only have at most %i", space_needed, space_available)
		return
	end

	for row = 0, vertical_space - 1 do
		for column = 0, ram_width - 1 do
			local xx, yy = filt(column, row)
			local id = sim.partID(xx, yy)
			if row >= 2 ^ ram_height_order then
				if id then
					sim.partKill(id)
				end
			else
				if not id then
					id = sim.partCreate(-3, xx, yy, elem.DEFAULT_PT_FILT)
					if id == -1 then
						printf.err("out of particle IDs")
						return
					end
				end
				local index = row * ram_width + column
				local opcode = opcodes[index] and opcodes[index].dwords[1] or nop.dwords[1]
				sim.partProperty(id, "ctype", opcode)
			end
		end
	end

	sim.partProperty(sim.partID(x - 4, y + 1), "ctype", 0x10000000 + 2 ^ (ram_height_order + 1) - 1)
end

return {
	includes = includes,
	dw_bits = dw_bits,
	nop = nop,
	entities = entities,
	mnemonics = mnemonics,
	flash = flash,
}

end

modules["tptasm.archs.b29k1qs60"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local detect = require("tptasm.detect")

local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_
		
		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap]
	end)
}

local dw_bits = 29 -- * TODO: figure out how dw even works here

local nop = opcode.make(192)
	:merge(0x20000000,   0)
	:merge(0x20000000,  32)
	:merge(0x20000000,  64)
	:merge(0x20000000,  96)
	:merge(0x20000000, 128)
	:merge(0x20000000, 160)

local entities = {}

local jmem_mnemonics = {
	["mvaz"] = { code = 0x20000000, jump = false, mask = false },
	["mvjz"] = { code = 0x20000001, jump =  true, mask = false },
	["ldaz"] = { code = 0x20000002, jump = false, mask =  true },
	["ldjz"] = { code = 0x20000003, jump =  true, mask =  true },
	["staz"] = { code = 0x20000004, jump = false, mask = false },
	["stjz"] = { code = 0x20000005, jump =  true, mask = false },
	["exaz"] = { code = 0x20000006, jump = false, mask = false },
	["exjz"] = { code = 0x20000007, jump =  true, mask = false },
}

local mnemonic_desc = {}

function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token, parameters)
	local parameter_ix = 0
	local function take_parameter(role)
		parameter_ix = parameter_ix + 1
		if not parameters[parameter_ix] then
			mnemonic_token:blamef(printf.err, "%s not specified", role)
			return false
		end
		local parameter = parameters[parameter_ix]
		if #parameter < 1 then
			parameter.before:blamef(printf.err, "no tokens in %s", role)
			return false
		end
		if #parameter > 1 then
			parameter[2]:blamef(printf.err, "excess tokens in %s", role)
			return false
		end
		if not parameter[1]:number() then
			parameter[1]:blamef(printf.err, "%s is not a number", role)
			return false
		end
		return parameter[1].parsed
	end

	local final_code = nop:clone()
	local instr_desc = jmem_mnemonics[mnemonic_token.value]
	final_code:merge(instr_desc.code, 128)
	do
		local set = take_parameter("bits to set")
		if not set then
			return false
		end
		final_code:merge(set, 0)
	end
	do
		local reset = take_parameter("bits to reset")
		if not reset then
			return false
		end
		final_code:merge(reset, 32)
	end
	if instr_desc.jump then
		do
			local condition = take_parameter("jump condition")
			if not condition then
				return false
			end
			final_code:merge(condition, 64)
		end
		do
			local target = take_parameter("jump target")
			if not target then
				return false
			end
			final_code:merge(target, 96)
		end
	end
	if instr_desc.mask and not instr_desc.jump then
		do
			local mask = take_parameter("read mask")
			if not mask then
				return false
			end
			final_code:merge(mask, 64)
		end
	end
	return true, { final_code }
end

local mnemonics = {}
for key in pairs(jmem_mnemonics) do
	mnemonics[key] = mnemonic_desc
end

local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end
	local space_available = 0x100
	if #opcodes >= space_available then
		printf.err("out of space; code takes %i cells, only have %i", #opcodes + 1, space_available)
		return
	end
	for ix = 0, #opcodes do
		for iy = 1, 6 do
			sim.partProperty(sim.partID(x + ix, y + iy + 3), "ctype", opcodes[ix].dwords[iy])
		end
	end
end

return {
	includes = includes,
	dw_bits = dw_bits,
	nop = nop,
	entities = entities,
	mnemonics = mnemonics,
	flash = flash,
}

end

modules["tptasm.archs.hacks"] = function()
local printf = require("tptasm.printf")

local function pig(mnemonic_token_hax, parameters_hax, mnemonics)
	local sub_instructions = {}

	local tokens = { mnemonic_token_hax }
	for _, parameter in ipairs(parameters_hax) do
		for _, token in ipairs(parameter) do
			table.insert(tokens, token)
		end
	end

	while tokens[1] do
		local mnemonic_token = tokens[1]
		table.remove(tokens, 1)

		local instr_desc = mnemonics[mnemonic_token.value]
		if not instr_desc then
			mnemonic_token:blamef(printf.err, "unknown mnemonic")
			return false
		end

		local wants_operands = tonumber(instr_desc.class:sub(1, 1))
		local operand
		do
			local operand_list = {}
			while tokens[1] and not tokens[1]:punctuator("|") do
				table.insert(operand_list, tokens[1])
				table.remove(tokens, 1)
			end
			if tokens[1] and tokens[1]:punctuator("|") then
				table.remove(tokens, 1)
			end

			if #operand_list > wants_operands then
				operand_list[wants_operands + 1]:blamef(printf.err, "excess operands")
				return false
			end
			if #operand_list < wants_operands then
				if #operand_list == 0 then
					mnemonic_token:blamef_after(printf.err, "insufficient operands")
				else
					operand_list[#operand_list]:blamef_after(printf.err, "insufficient operands")
				end
				return false
			end
			operand = operand_list[1]
		end

		if operand then
			local expect_class = "X"
			if operand:number() then
				expect_class = "I"
			end
			if operand:is("entity") then
				expect_class = "R"
			end
			if not instr_desc.class:find(expect_class) then -- * it can only be a register
				mnemonic_token:blamef(printf.err, "no variant of '%s' exists that takes a %s operand '%s'", mnemonic_token.value, operand.type, operand.value)
				return false
			end
		end

		table.insert(sub_instructions, {
			instr_desc = instr_desc,
			mnemonic_token = mnemonic_token,
			operand = operand
		})
	end

	return sub_instructions
end

return {
	pig = pig, -- * Aka piped instruction groups. (Stupid way to say that multiple instructions exist on the same source line and are delimited by '|'.)
}

end

modules["tptasm.archs.i8m7d28s"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local detect = require("tptasm.detect")
local xbit32 = require("tptasm.xbit32")
local hacks  = require("tptasm.archs.hacks")

local arch_i8m7d28s = {}

local local_config = {
	org_data_base = 0x100
}
local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_

		%define dw `dw'
		%define org `org'

		%define org_data_base `org_data_base'

		%macro org_data origin
			org { org_data_base origin + }
		%endmacro

		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap] or local_config[cap]
	end)
}

local entities = {
	-- * lol nothing
}

local mnemonics = {
	[   "nop"] = { class =  "0", value = 0x000, flags = "clam " }, -- * c: control slot
	[   "pop"] = { class =  "0", value = 0x200, flags = "   m " }, -- * l: logical slot
	[  "stsp"] = { class =  "0", value = 0x400, flags = "   m " }, -- * a: arithmetical slot
	[  "push"] = { class =  "0", value = 0x600, flags = "   m " }, -- * m: memory slot
	[   "ldm"] = { class = "1I", value = 0x800, flags = "   mp" }, -- * p: write to and lock P
	[    "ld"] = { class = "1I", value = 0x800, flags = "q  mp" }, -- * s: redirect explicit branch ptr to P
	["puship"] = { class =  "0", value = 0xA00, flags = "   m " }, -- * b: write to and lock EBP^
	[   "stm"] = { class = "1I", value = 0xC00, flags = "   mp" }, -- * n: write to and lock IP
	[    "st"] = { class = "1I", value = 0xC00, flags = "q  mp" }, -- * o: write to in/out 1-bit offset
	[ "pushi"] = { class = "1I", value = 0xE00, flags = "   mp" }, -- * q: subtract org_data_base from immediate
	[   "sta"] = { class =  "0", value = 0x040, flags = "  a  " },
	[   "dec"] = { class =  "0", value = 0x080, flags = "  a  " },
	[   "inc"] = { class =  "0", value = 0x0C0, flags = "  a  " },
	[  "subi"] = { class = "1I", value = 0x100, flags = "  a p" },
	[  "addi"] = { class = "1I", value = 0x140, flags = "  a p" },
	[   "sub"] = { class =  "0", value = 0x180, flags = "  a  " },
	[   "add"] = { class =  "0", value = 0x1C0, flags = "  a  " },
	[    "or"] = { class =  "0", value = 0x008, flags = " l   " },
	[   "and"] = { class =  "0", value = 0x010, flags = " l   " },
	[   "xor"] = { class =  "0", value = 0x018, flags = " l   " },
	[    "rr"] = { class =  "0", value = 0x020, flags = " l   " },
	[    "rl"] = { class =  "0", value = 0x028, flags = " l   " },
	[   "rng"] = { class =  "0", value = 0x030, flags = " l   " },
	[   "stl"] = { class =  "0", value = 0x038, flags = " l   " },
	[    "jp"] = { class = "1I", value = 0x000, flags = "  b  " },
	[   "jpb"] = { class =  "0", value = 0x001, flags = "c    " },
	[    "jz"] = { class = "1I", value = 0x002, flags = "c   p" },
	[    "jc"] = { class = "1I", value = 0x003, flags = "c   p" },
	[   "jnz"] = { class = "1I", value = 0x002, flags = "c ns " },
	[   "jnc"] = { class = "1I", value = 0x003, flags = "c ns " },
	[   "out"] = { class = "1I", value = 0x004, flags = "co   " },
	[    "in"] = { class = "1I", value = 0x005, flags = "co   " },
}

local nop = opcode.make(28):merge(0x20000000, 0)

local dw_bits = 29

local clam_slot_name = {
	["c"] = "control",
	["l"] = "logical",
	["a"] = "arithmetical",
	["m"] = "memory"
}

local mnemonic_desc = {}

function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token_hax, parameters_hax, offset)
	local sub_instructions = hacks.pig(mnemonic_token_hax, parameters_hax, mnemonics)
	if not sub_instructions then
		return false
	end

	local final_code = nop:clone()
	local clam_slot_used = {}
	local ip_locked = false
	local ip_value = 0
	local p_locked = false
	local p_value = 0
	local explicit_branch = false
	local branch_to_p = false
	for _, subinst in ipairs(sub_instructions) do
		if subinst.instr_desc.flags:find("s") then
			branch_to_p = true
		end
	end
	for _, subinst in ipairs(sub_instructions) do
		for clam_slot in subinst.instr_desc.flags:gmatch("[clam]") do
			if clam_slot_used[clam_slot] then
				subinst.mnemonic_token:blamef(printf.err, "micro-instruction slot '%s' used multiple times", clam_slot_name[clam_slot])
				clam_slot_used[clam_slot]:blamef(printf.info, "first used here")
				return false
			end
			clam_slot_used[clam_slot] = subinst.mnemonic_token
		end

		local imm
		if subinst.operand and subinst.operand:number() then
			imm = subinst.operand.parsed
			local bits = subinst.instr_desc.flags:find("o") and 1 or 8
			local trunc = 2 ^ bits
			if subinst.instr_desc.flags:find("q") then
				imm = imm - local_config.org_data_base
			end
			if imm >= trunc then
				imm = imm % trunc
				subinst.operand:blamef(printf.warn, "number truncated to %i bits", bits)
			end
		end

		local temp_flags = subinst.instr_desc.flags
		if temp_flags:find("b") then
			explicit_branch = true
			if branch_to_p then
				temp_flags = temp_flags .. "p"
			else
				temp_flags = temp_flags .. "n"
			end
		end

		if temp_flags:find("p") then
			if p_locked and subinst.operand.parsed ~= p_value then
				subinst.mnemonic_token:blamef(printf.err, "general pointer already locked")
				p_locked:blamef(printf.info, "locked by this")
				return false
			end
			p_locked = subinst.mnemonic_token
			p_value = imm
		end

		if temp_flags:find("n") then
			if ip_locked and subinst.operand.parsed ~= ip_value then
				subinst.mnemonic_token:blamef(printf.err, "instruction pointer already locked")
				ip_locked:blamef(printf.info, "locked by this")
				return false
			end
			ip_locked = subinst.mnemonic_token
			ip_value = imm
		end

		if temp_flags:find("o") then
			final_code:merge(imm, 1)
		end

		final_code:merge(subinst.instr_desc.value, 0)
	end
	if not explicit_branch then
		local next_inst = xbit32.band(offset + 1, 0xFF)
		if branch_to_p then
			p_value = next_inst
		else
			ip_value = next_inst
		end
	end
	final_code:merge(p_value, 12)
	final_code:merge(ip_value, 20)

	return true, { final_code }
end

local mnemonics = {}
for key in pairs(mnemonics) do
	mnemonics[key] = mnemonic_desc
end

local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end

	local space_available = 0x180
	if #opcodes >= space_available then
		printf.err("out of space; code takes %i cells, only have %i", #opcodes + 1, space_available)
		return
	end

	do
		local frame_pos = -1
		while sim.partProperty(sim.partID(x + frame_pos, y + 3), "type") == elem.DEFAULT_PT_PSTN do
			frame_pos = frame_pos + 1
		end
		for ix = 0x00, 0xFF do
			local code = opcodes[ix] and opcodes[ix].dwords[1] or 0x20000000
			local column = ix % 32
			local row = (ix - column) / 32
			sim.partProperty(sim.partID(x + frame_pos + 2 + column, y + 5 + row), "ctype", code)
		end
	end
	do
		local frame_pos = -3
		while sim.partProperty(sim.partID(x + frame_pos, y + 50), "type") == elem.DEFAULT_PT_PSTN do
			frame_pos = frame_pos + 1
		end
		for ix = 0x00, 0x7F do
			local data = opcodes[ix + 0x100] and opcodes[ix + 0x100].dwords[1] or 0x20000000
			local column = ix % 16
			local row = (ix - column) / 16
			sim.partProperty(sim.partID(x + frame_pos + 2 + column, y + 57 + row), "ctype", data)
		end
	end
end

return {
	includes = includes,
	dw_bits = dw_bits,
	nop = nop,
	entities = entities,
	mnemonics = mnemonics,
	flash = flash,
}

end

modules["tptasm.archs.maps"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local detect = require("tptasm.detect")
local xbit32 = require("tptasm.xbit32")

local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_

		%define dw `dw'
		%define org `org'

		%macro jz Label
			jnzg . `peerlabel' _Jz `macrounique', Label
		. `peerlabel' _Jz `macrounique':
		%endmacro

		%macro jzg Label, GLabel
			jnzg GLabel, Label
		%endmacro

		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap]
	end)
}

local entities = {}
local nop = opcode.make(18)
local dw_bits = 18

local mnemonic_to_class_code_g = {
	[  "st" ] = { class = "S1", code = 0x00000 },
	[  "gt" ] = { class =  "1", code = 0x00280 },
	[  "lt" ] = { class =  "1", code = 0x00200 },
	[ "jnz" ] = { class = "C1", code = 0x00400 },
	[  "ld" ] = { class =  "1", code = 0x00680 },
	[ "xor" ] = { class =  "1", code = 0x01080 },
	[ "add" ] = { class =  "1", code = 0x01280 },
	[ "and" ] = { class =  "1", code = 0x01400 },
	[  "or" ] = { class =  "1", code = 0x01680 },
	[ "shl" ] = { class =  "0", code = 0x00900 },
	[ "shr" ] = { class =  "0", code = 0x00B00 },
}
local mnemonic_to_class_code = {}
for mnemonic, class_code in pairs(mnemonic_to_class_code_g) do
	mnemonic_to_class_code[mnemonic       ] = { class = class_code.class       , code = class_code.code }
	mnemonic_to_class_code[mnemonic .. "g"] = { class = class_code.class .. "G", code = class_code.code }
end
local mnemonic_desc = {}

function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token, parameters, offset)
	local operands = {}
	for ix, ix_param in ipairs(parameters) do
		if #ix_param == 3
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:number()
		   and ix_param[3]:punctuator("]") then
			table.insert(operands, {
				type = "mem",
				value = ix_param[2].parsed,
				token = ix_param[2]
			})

		elseif #ix_param == 1
		   and ix_param[1]:number() then
			table.insert(operands, {
				type = "imm",
				value = ix_param[1].parsed,
				token = ix_param[1]
			})
		   
		else
			if ix_param[1] then
				ix_param[1]:blamef(printf.err, "operand format not recognised")
			else
				ix_param.before:blamef_after(printf.err, "operand format not recognised")
			end
			return false

		end
	end

	local desc = mnemonic_to_class_code[mnemonic_token.value]
	local operand_mode_valid = false
	local wants_operands = (desc.class:find("1") and 1 or 0) + (desc.class:find("G") and 1 or 0)
	if #operands == wants_operands then
		operand_mode_valid = true
	end
	if wants_operands == 1 and desc.class:find("C") and operands[1].type ~= "imm" then
		operand_mode_valid = false
	end
	if not operand_mode_valid then
		local operands_repr = {}
		for _, ix_oper in ipairs(operands) do
			table.insert(operands_repr, ix_oper.type)
		end
		mnemonic_token:blamef(printf.err, "no variant of %s exists that takes '%s' operands", mnemonic_token.value, table.concat(operands_repr, ", "))
		return false
	end

	local goto_target = offset + 1
	local imm_operand = 0
	if desc.class:find("G") then
		local operand = operands[#operands]
		local truncated = operand.value % 0x80
		local value = operand.value
		if value >= 0x80 then
			value = value % 0x80
			operand.token:blamef(printf.warn, "number truncated to 7 bits")
		end
		goto_target = value
		operands[#operands] = nil
	end
	if desc.class:find("1") then
		local limit = desc.class:find("C") and 7 or 5
		local operand = operands[#operands]
		local truncated = operand.value % 2 ^ limit
		local value = operand.value
		if value >= 2 ^ limit then
			value = value % 2 ^ limit
			operand.token:blamef(printf.warn, "number truncated to " .. limit .. " bits")
		end
		imm_operand = value
		if operand.type == "mem" and imm_operand >= 0x10 and not desc.class:find("S") then
			imm_operand = imm_operand % 2 * 2 + 0x10
		end
		if operand.type == "imm" and not desc.class:find("C") then
			imm_operand = xbit32.band(xbit32.rshift(imm_operand, 4), 1) + xbit32.band(xbit32.rshift(imm_operand, 2), 2) + xbit32.lshift(xbit32.band(imm_operand, 3), 3) + xbit32.band(imm_operand, 4) + 0x20
		end
	end

	local final_code = opcode.make(18):merge(desc.code, 0)
	if desc.class:find("C") then
		final_code:merge(xbit32.band(xbit32.rshift(imm_operand, 6), 1),  7)
		final_code:merge(xbit32.band(xbit32.rshift(imm_operand, 5), 1), 13)
		final_code:merge(xbit32.band(xbit32.rshift(imm_operand, 4), 1), 11)
		final_code:merge(xbit32.band(              imm_operand    , 1), 15)
		final_code:merge(xbit32.band(xbit32.rshift(imm_operand, 1), 1), 17)
		final_code:merge(xbit32.band(xbit32.rshift(imm_operand, 2), 1), 16)
		final_code:merge(xbit32.band(xbit32.rshift(imm_operand, 3), 1), 14)
	else
		final_code:merge(xbit32.band(              imm_operand    , 15), 14)
		final_code:merge(xbit32.band(xbit32.rshift(imm_operand, 4),  1), 13)
		final_code:merge(xbit32.band(xbit32.rshift(imm_operand, 5),  1), 11)
	end
	final_code:merge(xbit32.band(xbit32.rshift(goto_target, 5), 1), 0)
	final_code:merge(xbit32.band(xbit32.rshift(goto_target, 4), 1), 1)
	final_code:merge(xbit32.band(xbit32.rshift(goto_target, 1), 1), 2)
	final_code:merge(xbit32.band(              goto_target    , 1), 3)
	final_code:merge(xbit32.band(xbit32.rshift(goto_target, 2), 1), 4)
	final_code:merge(xbit32.band(xbit32.rshift(goto_target, 6), 1), 5)
	final_code:merge(xbit32.band(xbit32.rshift(goto_target, 3), 1), 6)

	return true, { final_code }
end

local mnemonics = {}
for key in pairs(mnemonic_to_class_code) do
	mnemonics[key] = mnemonic_desc
end

local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end

	local space_available = 0x80
	if #opcodes >= space_available then
		printf.err("out of space; code takes %i cells, only have %i", #opcodes + 1, space_available)
		return
	end

	for ix = 0, 127 do
		local opcode = opcodes[ix] and opcodes[ix].dwords[1] or 0x00E00
		local x_ = x + bit.band(bit.rshift(ix, 6), 1)
		local y_ = y + 1 + bit.band(ix, 63)
		for ib = 0, 17 do
			local old = sim.partID(x_ + ib * 2, y_)
			if old then
				sim.partKill(old)
			end
			if bit.band(opcode, bit.lshift(1, ib)) ~= 0 then
				local new = sim.partCreate(-3, x_ + ib * 2, y_, elem.DEFAULT_PT_FILT)
				sim.partProperty(new, "ctype", 0x20000001)
			end
		end
	end
end

return {
	includes = includes,
	dw_bits = dw_bits,
	nop = nop,
	entities = entities,
	mnemonics = mnemonics,
	flash = flash,
}

end

modules["tptasm.archs.micro21"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local detect = require("tptasm.detect")
local xbit32 = require("tptasm.xbit32")

local macros_str
local macros_arr = {}
for macro, code in pairs({
	[  "c"] = "if  s,  1",
	[  "a"] = "if  s,  2",
	[  "e"] = "if  s,  4",
	[  "b"] = "ifn s,  6",
	[ "2z"] = "if  s,  8",
	[ "1z"] = "if  s, 16",
	[ "ly"] = "if  s, 32",
	[ "nc"] = "ifn s,  1",
	[ "na"] = "ifn s,  2",
	[ "ne"] = "ifn s,  4",
	[ "nb"] = "if  s,  6",
	["n2z"] = "ifn s,  8",
	["n1z"] = "ifn s, 16",
	[ "ln"] = "ifn s, 32",
}) do
	table.insert(macros_arr, ([==[
		%%macro if%s
			%s
		%%endmacro
	]==]):format(macro, code))
	table.insert(macros_arr, ([==[
		%%macro j%s loc
			if%s
			jmp loc
		%%endmacro
	]==]):format(macro, macro))
end
macros_str = table.concat(macros_arr)

local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_

		%define dw `dw'
		%define org `org'

		%macro ifz reg
			ifn reg, 0xFF
		%endmacro

		%macro ifnz reg
			if reg, 0xFF
		%endmacro
	]==] .. macros_str .. [==[

		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap]
	end)
}

local dw_bits = 17

local nop = opcode.make(17)

local entities = {
	["a"] = { type = "register", offset = 1 },
	["b"] = { type = "register", offset = 2 },
	["s"] = { type = "register", offset = 3 },
}

local mnemonics = {}
local mnemonic_to_class_code = {
	[ "stop"] = { class = "nop", code = 0x00000 },
	["stopv"] = { class = "nop", code = 0x00FFF },
	[  "jmp"] = { class =   "0", code = 0x01000 },
	[   "if"] = { class =  "01", code = 0x02000 },
	[  "ifn"] = { class =  "01", code = 0x03000 },
	[  "nin"] = { class =   "0", code = 0x04000 },
	[ "copy"] = { class =  "01", code = 0x05000 },
	[  "lin"] = { class = "nop", code = 0x06000 },
	[  "rnd"] = { class = "nop", code = 0x07000 },
	[  "adc"] = { class =  "01", code = 0x08000 },
	[  "add"] = { class =  "01", code = 0x09000 },
	[  "lod"] = { class =  "01", code = 0x0A000 },
	[  "sto"] = { class =  "01", code = 0x0B000 },
	[  "and"] = { class =  "01", code = 0x0C000 },
	[   "or"] = { class =  "01", code = 0x0D000 },
	[  "xor"] = { class =  "01", code = 0x0E000 },
	[  "shr"] = { class =  "01", code = 0x10000 },
	[  "shl"] = { class =  "01", code = 0x11000 },
	[  "rtr"] = { class =  "01", code = 0x12000 },
	[  "rtl"] = { class =  "01", code = 0x13000 },
	[  "out"] = { class =  "0?", code = 0x14000 },
	[ "stip"] = { class = "nop", code = 0x15000 },
	[ "test"] = { class =  "01", code = 0x16000 },
	[ "sysr"] = { class = "nop", code = 0x17000 },
	[  "neg"] = { class =  "01", code = 0x18000 },
	[  "nop"] = { class = "nop", code = 0x1F000 },
	[ "nopv"] = { class = "nop", code = 0x1FFFF },
}

local mnemonic_desc = {}

function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token, parameters)
	local operands = {}
	for ix, ix_param in ipairs(parameters) do
		if #ix_param == 1
		   and ix_param[1]:is("entity") and ix_param[1].entity.type == "register" then
			table.insert(operands, {
				type = "reg",
				value = ix_param[1].entity.offset
			})

		elseif #ix_param == 1
		   and ix_param[1]:number() then
			table.insert(operands, {
				type = "imm",
				value = ix_param[1].parsed,
				token = ix_param[1]
			})
		   
		else
			if ix_param[1] then
				ix_param[1]:blamef(printf.err, "operand format not recognised")
			else
				ix_param.before:blamef_after(printf.err, "operand format not recognised")
			end
			return false

		end
	end

	local desc = mnemonic_to_class_code[mnemonic_token.value]
	local operand_mode_valid = false
	if (desc.class == "nop" and #operands == 0)
	or ((desc.class == "0" or desc.class == "0?") and #operands == 1)
	or ((desc.class == "01" or desc.class == "0?") and #operands == 2) then
		operand_mode_valid = true
	end
	if #operands == 2 and operands[1].type == "imm" and operands[2].type == "imm" then
		operand_mode_valid = false
	end
	if not operand_mode_valid then
		local operands_repr = {}
		for _, ix_oper in ipairs(operands) do
			table.insert(operands_repr, ix_oper.type)
		end
		mnemonic_token:blamef(printf.err, "no variant of %s exists that takes '%s' operands", mnemonic_token.value, table.concat(operands_repr, ", "))
		return false
	end

	local final_code = opcode.make(17):merge(desc.code, 0)
	local function bind_operand(slot, operand)
		if operand.type == "imm" then
			local truncated = operand.value % 0x100
			local value = operand.value
			if value >= 0x100 then
				value = value % 0x100
				operands[ix].token:blamef(printf.warn, "number truncated to 8 bits")
			end
			final_code:merge(value, 0)
		elseif operand.type == "reg" then
			final_code:merge(operand.value, 12 - slot * 2)
		end
	end
	for ix, ix_op in ipairs(operands) do
		bind_operand(ix, ix_op)
	end
	return true, { final_code }
end
for mnemonic in pairs(mnemonic_to_class_code) do
	mnemonics[mnemonic] = mnemonic_desc
end

local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end
	local space_available = 0x80
	if #opcodes >= space_available then
		printf.err("out of space; code takes %i cells, only have %i", #opcodes + 1, space_available)
		return
	end
	local colour_codes = {
		[ 0] = 0xFF00FF00,
		[ 1] = 0xFF00FF00,
		[ 2] = 0xFF00FF00,
		[ 3] = 0xFF00FF00,
		[ 4] = 0xFF00FF00,
		[ 5] = 0xFF00FF00,
		[ 6] = 0xFF00FF00,
		[ 7] = 0xFF00FF00,
		[ 8] = 0xFFFF00FF,
		[ 9] = 0xFFFF00FF,
		[10] = 0xFFFFFF00,
		[11] = 0xFFFFFF00,
		[12] = 0xFFFF0000,
		[13] = 0xFFFF0000,
		[14] = 0xFFFF0000,
		[15] = 0xFFFF0000,
		[16] = 0xFFFF0000,
	}
	for iy = 0, 127 do
		local code = opcodes[iy] and opcodes[iy].dwords[1] or 0
		for ix = 0, 16 do
			local px, py = x - ix + 354, y + iy + 73
			local old_id = sim.partID(px, py)
			if old_id then
				sim.partKill(old_id)
			end
			if xbit32.band(code, 2 ^ ix) ~= 0 then
				local new_id = sim.partCreate(-2, px, py, elem.DEFAULT_PT_ARAY)
				local colour_code = colour_codes[ix]
				if code == 0x1FFFF or code == 0x00FFF then
					colour_code = 0xFF00FFFF
				end
				if xbit32.band(code, 0x1F000) == 0x01000 and ix < 8 then
					colour_code = 0xFF00FFFF
				end
				sim.partProperty(new_id, "dcolour", colour_code)
			end
		end
	end
end

return {
	includes = includes,
	dw_bits = dw_bits,
	nop = nop,
	entities = entities,
	mnemonics = mnemonics,
	flash = flash,
}

end

modules["tptasm.archs.ptp7"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local detect = require("tptasm.detect")

local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_

		%ifndef _CONST_MINUS_1
		% error "_CONST_MINUS_1 must be defined and must resolve to an address that holds the constant value 0x1FFFFFFF"
		%endif

		%define dw `dw'
		%define org `org'
		
		%macro jmp loc
			adc [_CONST_MINUS_1]
			jc loc
		%endmacro

		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap]
	end)
}

local nop = opcode.make(29)

local entities = {}

local mnemonic_to_class_code = {
	[ "shl"] = { class = "1V", code = 0x00 },
	[ "shr"] = { class = "1V", code = 0x01 },
	[ "add"] = { class = "1M", code = 0x02 },
	[ "adc"] = { class = "1M", code = 0x12 },
	["flip"] = { class = "0V", code = 0x03 },
	[ "mov"] = { class = "1M", code = 0x04 },
	["sync"] = { class = "0V", code = 0x05 },
	[ "nop"] = { class = "0V", code = 0x06 },
	[  "jc"] = { class = "1V", code = 0x07 },
}

local mnemonic_desc = {}

function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token, parameters)
	local final_code = nop:clone()
	local instr_desc = mnemonic_to_class_code[mnemonic_token.value]
	final_code:merge(instr_desc.code, 24)

	local operands = {}
	for ix, ix_param in ipairs(parameters) do
		if #ix_param == 1
		   and ix_param[1]:number() then
			table.insert(operands, {
				type = "imm",
				value = ix_param[1].parsed,
				token = ix_param[1]
			})

		elseif #ix_param == 3
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:number()
		   and ix_param[3]:punctuator("]") then
			table.insert(operands, {
				type = "immaddr",
				value = ix_param[2].parsed,
				token = ix_param[2]
			})
		   
		else
			if ix_param[1] then
				ix_param[1]:blamef(printf.err, "operand format not recognised")
			else
				ix_param.before:blamef_after(printf.err, "operand format not recognised")
			end
			return false

		end
	end

	local operand_mode_valid = false
	if instr_desc.class == "0V" then
		if #operands == 0 then
			operand_mode_valid = true
		end

	elseif instr_desc.class == "1V" then
		if #operands == 1 and operands[1].type == "imm" then
			operand_mode_valid = true
		end

	elseif instr_desc.class == "1M" then
		if #operands == 1 and operands[1].type == "immaddr" then
			operand_mode_valid = true
		end

	end
	if not operand_mode_valid then
		local operands_repr = {}
		for _, ix_oper in ipairs(operands) do
			table.insert(operands_repr, ix_oper.type)
		end
		mnemonic_token:blamef(printf.err, "no variant of %s exists that takes '%s' operands", mnemonic_token.value, table.concat(operands_repr, ", "))
		return false
	end

	if #operands == 1 then
		local value = operands[1].value
		if value >= 0x1000000 then
			value = value % 0x1000000
			operands[1].token:blamef(printf.warn, "number truncated to 24 bits")
		end
		final_code:merge(value, 0)
	end

	return true, { final_code }
end

local dw_bits = 29

local mnemonics = {}
for key in pairs(mnemonic_to_class_code) do
	mnemonics[key] = mnemonic_desc
end

local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end
	local space_available = 0x1000
	if #opcodes >= space_available then
		printf.err("out of space; code takes %i cells, only have %i", #opcodes + 1, space_available)
		return
	end
	for ix = 0, 0xFFF do
		sim.partProperty(sim.partID(x + (ix % 128) - 1, y + math.floor(ix / 128) - 34), "ctype", 0x20000000 + (opcodes[ix] and opcodes[ix].dwords[1] or 0))
	end
end

return {
	includes = includes,
	dw_bits = dw_bits,
	nop = nop,
	entities = entities,
	mnemonics = mnemonics,
	flash = flash,
}

end

modules["tptasm.archs.r2"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local detect = require("tptasm.detect")

local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_
		
		%define dw `dw'
		%define org `org'

		%define ja jnbe
		%define jna jbe
		%define jae jnc
		%define jnae jc
		%define je jz
		%define jne jnz
		%define jnle jg
		%define jle jng
		%define jnl jge
		%define jl jnge
		%define jb jc
		%define jnb jnc
		%define test ands
		%define cmb sbbs
		%define cmp subs

		%macro nop
			jn r0
		%endmacro

		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap]
	end)
}

local dw_bits = 29

local nop = opcode.make(32):merge(0x20000000, 0)

local entities = {
	[  "r0" ] = { type = "register", offset =  0 },
	[  "r1" ] = { type = "register", offset =  1 },
	[  "r2" ] = { type = "register", offset =  2 },
	[  "r3" ] = { type = "register", offset =  3 },
	[  "r4" ] = { type = "register", offset =  4 },
	[  "r5" ] = { type = "register", offset =  5 },
	[  "r6" ] = { type = "register", offset =  6 },
	[  "r7" ] = { type = "register", offset =  7 },
	[  "r8" ] = { type = "register", offset =  8 },
	[  "r9" ] = { type = "register", offset =  9 },
	[ "r10" ] = { type = "register", offset = 10 },
	[ "r11" ] = { type = "register", offset = 11 },
	[ "r12" ] = { type = "register", offset = 12 },
	[ "r13" ] = { type = "register", offset = 13 },
	[ "r14" ] = { type = "register", offset = 14 },
	[ "r15" ] = { type = "register", offset = 15 },
}
entities["bp"] = entities["r13"]
entities["sp"] = entities["r14"]
entities["ip"] = entities["r15"]

local mnemonics = {}

local mnemonic_to_class_code = {
    [ "mov"  ] = { class = "AB", code = 0x20000000 },
    [ "and"  ] = { class = "AB", code = 0x21000000 },
    [ "or"   ] = { class = "AB", code = 0x22000000 },
    [ "xor"  ] = { class = "AB", code = 0x23000000 },
    [ "add"  ] = { class = "AB", code = 0x24000000 },
    [ "adc"  ] = { class = "AB", code = 0x25000000 },
    [ "sub"  ] = { class = "AB", code = 0x26000000 },
    [ "sbb"  ] = { class = "AB", code = 0x27000000 },
    [ "swm"  ] = { class = " B", code = 0x28000000 },
    [ "ands" ] = { class = "AB", code = 0x29000000 },
    [ "ors"  ] = { class = "AB", code = 0x2A000000 },
    [ "xors" ] = { class = "AB", code = 0x2B000000 },
    [ "adds" ] = { class = "AB", code = 0x2C000000 },
    [ "adcs" ] = { class = "AB", code = 0x2D000000 },
    [ "subs" ] = { class = "AB", code = 0x2E000000 },
    [ "sbbs" ] = { class = "AB", code = 0x2F000000 },
    [ "hlt"  ] = { class = "  ", code = 0x30000000 },
    [ "jmp"  ] = { class = " B", code = 0x31000000 },
    [ "jn"   ] = { class = " B", code = 0x31000001 },
    [ "jc"   ] = { class = " B", code = 0x31000002 },
    [ "jnc"  ] = { class = " B", code = 0x31000003 },
    [ "jo"   ] = { class = " B", code = 0x31000004 },
    [ "jno"  ] = { class = " B", code = 0x31000005 },
    [ "js"   ] = { class = " B", code = 0x31000006 },
    [ "jns"  ] = { class = " B", code = 0x31000007 },
    [ "jz"   ] = { class = " B", code = 0x31000008 },
    [ "jnz"  ] = { class = " B", code = 0x31000009 },
    [ "jng"  ] = { class = " B", code = 0x3100000A },
    [ "jg"   ] = { class = " B", code = 0x3100000B },
    [ "jnge" ] = { class = " B", code = 0x3100000C },
    [ "jge"  ] = { class = " B", code = 0x3100000D },
    [ "jbe"  ] = { class = " B", code = 0x3100000E },
    [ "jnbe" ] = { class = " B", code = 0x3100000F },
    [ "rol"  ] = { class = "AB", code = 0x32000000 },
    [ "ror"  ] = { class = "AB", code = 0x33000000 },
    [ "shl"  ] = { class = "AB", code = 0x34000000 },
    [ "shr"  ] = { class = "AB", code = 0x35000000 },
    [ "scl"  ] = { class = "AB", code = 0x36000000 },
    [ "scr"  ] = { class = "AB", code = 0x37000000 },
    [ "bump" ] = { class = "A ", code = 0x38000000 },
    [ "wait" ] = { class = "A ", code = 0x39000000 },
    [ "send" ] = { class = "AB", code = 0x3A000000 },
    [ "recv" ] = { class = "AB", code = 0x3B000000 },
    [ "push" ] = { class = " B", code = 0x3C000000 },
    [ "pop"  ] = { class = "A ", code = 0x3D000000 },
    [ "call" ] = { class = " B", code = 0x3E000000 },
    [ "ret"  ] = { class = "  ", code = 0x3F000000 },
}
local class_to_mode = {
    ["  "] = {
        { ops = {                               }, code = 0x00000000 },
    },
    ["A "] = {
        { ops = {  "reg@0"                      }, code = 0x00000000 },
        { ops = { "[reg@0]"                     }, code = 0x00400000 },
        { ops = { "[reg@16+reg@0]"              }, code = 0x00C00000 },
        { ops = { "[reg@16-reg@0]"              }, code = 0x00C08000 },
        { ops = { "[imm16@4]"                   }, code = 0x00500000 },
        { ops = { "[imm11@4+reg@16]"            }, code = 0x00D00000 },
        { ops = { "[reg@16+imm11@4]"            }, code = 0x00D00000 },
        { ops = { "[reg@16-imm11@4]"            }, code = 0x00D08000 },
    },
    [" B"] = {
        { ops = {  "reg@4"                      }, code = 0x00000000 },
        { ops = { "[reg@4]"                     }, code = 0x00100000 },
        { ops = { "[reg@16+reg@4]"              }, code = 0x00900000 },
        { ops = { "[reg@16-reg@4]"              }, code = 0x00908000 },
        { ops = {  "imm16@4"                    }, code = 0x00200000 },
        { ops = { "[imm16@4]"                   }, code = 0x00300000 },
        { ops = { "[imm11@4+reg@16]"            }, code = 0x00B00000 },
        { ops = { "[reg@16+imm11@4]"            }, code = 0x00B00000 },
        { ops = { "[reg@16-imm11@4]"            }, code = 0x00B08000 },
    },
    ["AB"] = {
        { ops = {  "reg@0" ,  "reg@4"           }, code = 0x00000000 },
        { ops = {  "reg@0" , "[reg@4]"          }, code = 0x00100000 },
        { ops = {  "reg@0" , "[reg@16+reg@4]"   }, code = 0x00900000 },
        { ops = {  "reg@0" , "[reg@16-reg@4]"   }, code = 0x00908000 },
        { ops = {  "reg@0" ,  "imm16@4"         }, code = 0x00200000 },
        { ops = {  "reg@0" , "[imm16@4]"        }, code = 0x00300000 },
        { ops = {  "reg@0" , "[imm11@4+reg@16]" }, code = 0x00B00000 },
        { ops = {  "reg@0" , "[reg@16+imm11@4]" }, code = 0x00B00000 },
        { ops = {  "reg@0" , "[reg@16-imm11@4]" }, code = 0x00B08000 },
        { ops = { "[reg@0]",            "reg@4" }, code = 0x00400000 },
        { ops = { "[reg@16+reg@0]",     "reg@4" }, code = 0x00C00000 },
        { ops = { "[reg@16-reg@0]",     "reg@4" }, code = 0x00C08000 },
        { ops = { "[imm16@4]",          "reg@0" }, code = 0x00500000 },
        { ops = { "[imm11@4+reg@16]",   "reg@0" }, code = 0x00D00000 },
        { ops = { "[reg@16+imm11@4]",   "reg@0" }, code = 0x00D00000 },
        { ops = { "[reg@16-imm11@4]",   "reg@0" }, code = 0x00D08000 },
        { ops = { "[reg@0]",          "imm16@4" }, code = 0x00600000 },
        { ops = { "[reg@16+reg@0]",   "imm11@4" }, code = 0x00E00000 },
        { ops = { "[reg@16-reg@0]",   "imm11@4" }, code = 0x00E08000 },
        { ops = { "[imm16@4]",         "imm4@0" }, code = 0x00700000 },
        { ops = { "[imm11@4+reg@16]",  "imm4@0" }, code = 0x00F00000 },
        { ops = { "[reg@16+imm11@4]",  "imm4@0" }, code = 0x00F00000 },
        { ops = { "[reg@16-imm11@4]",  "imm4@0" }, code = 0x00F08000 },
    },
}

local mnemonic_desc = {}
function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token, parameters)
	local operands = {}
	for ix, ix_param in ipairs(parameters) do
		if #ix_param == 1
		   and ix_param[1]:is("entity") and ix_param[1].entity.type == "register" then
			table.insert(operands, {
				type = "reg",
				value = ix_param[1].entity.offset,
			})

		elseif #ix_param == 1
		   and ix_param[1]:number() then
			table.insert(operands, {
				type = "imm",
				value = ix_param[1].parsed,
				token = ix_param[1],
			})

		elseif #ix_param == 3
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:is("entity") and ix_param[2].entity.type == "register"
		   and ix_param[3]:punctuator("]") then
			table.insert(operands, {
				type = "[reg]",
				value = ix_param[2].entity.offset,
			})

		elseif #ix_param == 3
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:number()
		   and ix_param[3]:punctuator("]") then
			table.insert(operands, {
				type = "[imm]",
				value = ix_param[2].parsed,
				token = ix_param[2],
			})

		elseif #ix_param == 5
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:is("entity") and ix_param[2].entity.type == "register"
		   and ix_param[3]:punctuator("+")
		   and ix_param[4]:is("entity") and ix_param[4].entity.type == "register"
		   and ix_param[5]:punctuator("]") then
			table.insert(operands, {
				type = "[reg+reg]",
				base = ix_param[2].entity.offset,
				value = ix_param[4].entity.offset,
			})

		elseif #ix_param == 5
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:is("entity") and ix_param[2].entity.type == "register"
		   and ix_param[3]:punctuator("-")
		   and ix_param[4]:is("entity") and ix_param[4].entity.type == "register"
		   and ix_param[5]:punctuator("]") then
			table.insert(operands, {
				type = "[reg-reg]",
				base = ix_param[2].entity.offset,
				value = ix_param[4].entity.offset,
			})

		elseif #ix_param == 5
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:number()
		   and ix_param[3]:punctuator("+")
		   and ix_param[4]:is("entity") and ix_param[4].entity.type == "register"
		   and ix_param[5]:punctuator("]") then
			table.insert(operands, {
				type = "[reg+imm]",
				base = ix_param[4].entity.offset,
				value = ix_param[2].parsed,
				token = ix_param[2],
			})

		elseif #ix_param == 5
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:is("entity") and ix_param[2].entity.type == "register"
		   and ix_param[3]:punctuator("+")
		   and ix_param[4]:number()
		   and ix_param[5]:punctuator("]") then
			table.insert(operands, {
				type = "[reg+imm]",
				base = ix_param[2].entity.offset,
				value = ix_param[4].parsed,
				token = ix_param[4],
			})

		elseif #ix_param == 5
		   and ix_param[1]:punctuator("[")
		   and ix_param[2]:is("entity") and ix_param[2].entity.type == "register"
		   and ix_param[3]:punctuator("-")
		   and ix_param[4]:number()
		   and ix_param[5]:punctuator("]") then
			table.insert(operands, {
				type = "[reg-imm]",
				base = ix_param[2].entity.offset,
				value = ix_param[4].parsed,
				token = ix_param[4],
			})
		   
		else
			if ix_param[1] then
				ix_param[1]:blamef(printf.err, "operand format not recognised")
			else
				ix_param.before:blamef_after(printf.err, "operand format not recognised")
			end
			return false

		end
	end

	local final_code
	local class_code = mnemonic_to_class_code[mnemonic_token.value]
	if #class_code.class:gsub("[^AB]", "") == #operands then
		local modes = class_to_mode[class_code.class]
		for m = 1, #modes do
			local ops = modes[m].ops
			local ok = true
			for i = 1, #operands do
				if operands[i].type ~= ops[i]:gsub("[0-9@]", "") then
					ok = false
					break
				end
			end
			if ok then
				final_code = opcode.make(32):merge(class_code.code, 0):merge(modes[m].code, 0)
				for i = 1, #operands do
					local next_type = operands[i].type:gmatch("[a-z]+")
					local comp = 0
					for shift in ops[i]:gmatch("[0-9@]+") do
						comp = comp + 1
						local comp_type = next_type()
						local width = 4
						local value = operands[i].value
						if comp == 1 and operands[i].base then
							value = operands[i].base
						end
						if comp_type == "imm" then
							width, shift = shift:match("^([0-9]+)@([0-9]+)$")
							width = tonumber(width)
							shift = tonumber(shift)
						else
							shift = tonumber(shift:sub(2))
						end
						if value >= 2 ^ width then
							value = value % 2 ^ width
							operands[i].token:blamef(printf.warn, "number truncated to " .. width .. " bits")
						end
						final_code:merge(value, shift)
					end
				end
			end
		end
	end

	local hardware_bugs = false
	if mnemonic_token.value == "bump" and operands[1].type:find("^%[") then
		hardware_bugs = true
	end
	if mnemonic_token.value == "send" and operands[1].type:find("^%[") then
		hardware_bugs = true
	end
	if hardware_bugs then
		mnemonic_token:blamef(printf.err, "refusing to assemble due to hardware bugs, consult the manual")
		return false
	end

	if not final_code then
		local operands_repr = {}
		for _, ix_oper in ipairs(operands) do
			table.insert(operands_repr, ix_oper.type)
		end
		mnemonic_token:blamef(printf.err, "no variant of %s exists that takes '%s' operands", mnemonic_token.value, table.concat(operands_repr, ", "))
		return false
	end

	return true, { final_code }
end

for mnemonic in pairs(mnemonic_to_class_code) do
	mnemonics[mnemonic] = mnemonic_desc
end

local supported_models = {
	[ "R216K2A" ] = { ram_x = 70, ram_y =  -99, ram_width = 128, ram_height = 16 },
	[ "R216K4A" ] = { ram_x = 70, ram_y = -115, ram_width = 128, ram_height = 32 },
	[ "R216K8B" ] = { ram_x = 70, ram_y = -147, ram_width = 128, ram_height = 64 },
}
local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end

	local model_data = supported_models[model]
	local space_available = model_data.ram_width * model_data.ram_height
	if #opcodes >= space_available then
		printf.err("out of space; code takes %i cells, only have %i", #opcodes + 1, space_available)
		return
	end

	for row = 0, model_data.ram_height - 1 do
		local skipped = 0
		for column = 0, model_data.ram_width - 1 do
			while true do
				local id = sim.partID(x + model_data.ram_x - column - skipped, y + row + model_data.ram_y)
				if id and sim.partProperty(id, "type") ~= elem.DEFAULT_PT_FILT then
					id = nil
				end
				if id then
					local index = row * model_data.ram_width + column
					local opcode = opcodes[index] and opcodes[index].dwords[1] or nop.dwords[1]
					sim.partProperty(id, "ctype", opcode)
					break
				end
				if skipped > 0 then
					printf.err("RAM layout sanity check failed")
					return
				end
				skipped = 1
			end
		end
	end
end

return {
	includes = includes,
	dw_bits = dw_bits,
	nop = nop,
	entities = entities,
	mnemonics = mnemonics,
	flash = flash,
}

end

modules["tptasm.archs.r3"] = function()
local printf = require("tptasm.printf")
local config = require("tptasm.config")
local opcode = require("tptasm.opcode")
local xbit32 = require("tptasm.xbit32")
local detect = require("tptasm.detect")

local includes = {
	["common"] = ([==[
		%ifndef _COMMON_INCLUDED_
		%define _COMMON_INCLUDED_
		
		%define dw `dw'
		%define org `org'

		%define ja jnbe
		%define jna jbe
		%define jae jnc
		%define jnae jc
		%define je jz
		%define jne jnz
		%define jg jnle
		%define jng jle
		%define jge jnl
		%define jnge jl
		%define jb jc
		%define jnb jnc

		%define jya jynbe
		%define jyna jybe
		%define jyae jync
		%define jynae jyc
		%define jye jyz
		%define jyne jynz
		%define jyg jynle
		%define jyng jyle
		%define jyge jynl
		%define jynge jyl
		%define jyb jyc
		%define jynb jync

		%macro test pri, sec
			and r0, pri, sec
		%endmacro

		%macro cmp pri, sec
			sub r0, pri, sec
		%endmacro

		%macro nop
			mov r0, r0, r1
		%endmacro

		%endif ; _COMMON_INCLUDED_
	]==]):gsub("`([^\']+)'", function(cap)
		return config.reserved[cap]
	end)
}

local dw_bits = 32

local nop = opcode.make(32):merge(0x00000000, 0)

local entities = {}
for i = 0, 31 do
	entities["r" .. i] = { type = "register", offset = i }
end

local mnemonics = {}

local function transform(tbl, func)
	local newtbl = {}
	for key, value in pairs(tbl) do
		func(newtbl, key, value)
	end
	return newtbl
end

local function shallow_copy(tbl)
	return transform(tbl, function(newtbl, key, value)
		newtbl[key] = value
	end)
end

local cond_info = {
	[    "" ] = { code = 0x00000000 },
	[  "be" ] = { code = 0x00100000 },
	[   "l" ] = { code = 0x00200000 },
	[  "le" ] = { code = 0x00300000 },
	[   "s" ] = { code = 0x00400000 },
	[   "z" ] = { code = 0x00500000 },
	[   "o" ] = { code = 0x00600000 },
	[   "c" ] = { code = 0x00700000 },
	[   "n" ] = { code = 0x00800000 },
	[ "nbe" ] = { code = 0x00900000 },
	[  "nl" ] = { code = 0x00A00000 },
	[ "nle" ] = { code = 0x00B00000 },
	[  "ns" ] = { code = 0x00C00000 },
	[  "nz" ] = { code = 0x00D00000 },
	[  "no" ] = { code = 0x00E00000 },
	[  "nc" ] = { code = 0x00F00000 },
}
local mnemonic_info = {
	-- F: updates flags, append s to the mnemonic to suppress them
	-- G: can update flags, append f to the mnemonic to enable this
	-- J: some kind of jump (secondary is encoded as primary)
	-- H: some kind of shift (immediate is limited to 4 bits)
	-- P: takes primary
	-- S: takes secondary
	-- T: takes tertiary
	-- D: secondary defaults to primary
	-- M: secondary defaults to tertiary, or r0 if tertiary is imm
	-- E: secondary defaults to r0
	-- X: syntactically swap secondary and tertiary
	[  "mov" ] = { traits = "GPSTM  ", code = 0x00000000 },
	[    "j" ] = { traits = "J STE  ", code = 0x01010000 },
	[   "jy" ] = { traits = "J STE  ", code = 0x00010000 },
	[   "ld" ] = { traits = " PSTE  ", code = 0x00020000 },
	[  "exh" ] = { traits = "FPSTD  ", code = 0x00030000 },
	[  "sub" ] = { traits = "FPSTDX ", code = 0x00040000, companion_code_xor = 0x00020000, companion_add_one = true  },
	[  "sbb" ] = { traits = "FPSTDX ", code = 0x00050000, companion_code_xor = 0x00020000, companion_add_one = false },
	[  "add" ] = { traits = "FPSTD  ", code = 0x00060000 },
	[  "adc" ] = { traits = "FPSTD  ", code = 0x00070000 },
	[   "or" ] = { traits = "FPSTD  ", code = 0x00090000 },
	[  "xor" ] = { traits = "FPSTD  ", code = 0x00080000 },
	[   "st" ] = { traits = " PSTE  ", code = 0x000A0000 },
	[  "shl" ] = { traits = "FPSTD H", code = 0x000B0000 },
	[  "shr" ] = { traits = "FPSTD H", code = 0x000B8000 },
	[  "and" ] = { traits = "FPSTD  ", code = 0x000C0000 },
	[  "hlt" ] = { traits = "       ", code = 0x000D0000 },
	[  "mul" ] = { traits = " PST   ", code = 0x000E0000 },
	[ "mulh" ] = { traits = " PST   ", code = 0x000F0000 },
	[ "muls" ] = { traits = " PST   ", code = 0x800E0000 },
	[ "mulx" ] = { traits = " PST   ", code = 0x800F0000 },
}
mnemonic_info = transform(mnemonic_info, function(newtbl, key, value)
	if value.traits:find("F") then
		newtbl[key .. "s"] = value
		local nvalue = shallow_copy(value)
		nvalue.code = xbit32.bor(nvalue.code, 0x80000000)
		newtbl[key] = nvalue
	elseif value.traits:find("G") then
		newtbl[key] = value
		local nvalue = shallow_copy(value)
		nvalue.code = xbit32.bor(nvalue.code, 0x80000000)
		newtbl[key .. "f"] = nvalue
	else
		newtbl[key] = value
	end
end)
mnemonic_info = transform(mnemonic_info, function(newtbl, key, value)
	if value.traits:find("J") then
		for ckey, cvalue in pairs(cond_info) do
			local nvalue = shallow_copy(value)
			nvalue.code = xbit32.bor(nvalue.code, cvalue.code)
			local nkey = key .. ckey
			if nkey == "j" then
				nkey = "jmp"
			end
			newtbl[nkey] = nvalue
		end
	else
		newtbl[key] = value
	end
end)

local mnemonic_desc = {}
function mnemonic_desc.length()
	return true, 1 -- * RISC :)
end

function mnemonic_desc.emit(mnemonic_token, parameters)
	local operands = {}
	for ix, ix_param in ipairs(parameters) do
		if #ix_param == 1
		   and ix_param[1]:is("entity") and ix_param[1].entity.type == "register" then
			table.insert(operands, {
				type = "reg",
				value = ix_param[1].entity.offset
			})

		elseif #ix_param == 1
		   and ix_param[1]:number() then
			table.insert(operands, {
				type = "imm",
				value = ix_param[1].parsed,
				token = ix_param[1]
			})
		   
		else
			if ix_param[1] then
				ix_param[1]:blamef(printf.err, "operand format not recognised")
			else
				ix_param.before:blamef_after(printf.err, "operand format not recognised")
			end
			return false

		end
	end

	local info = mnemonic_info[mnemonic_token.value]
	local warnings_emitted = {}
	local named_ops = {}

	local code = info.code
	local named_op_index = 1
	local imm_index = 3
	if info.traits:find("X") then
		imm_index = 2
	end
	local imm_count = 0
	for ix_operand = 1, #operands do
		local operand = operands[ix_operand]
		if operand and operand.type == "imm" then
			imm_count = imm_count + 1
			local trunc_bits = 16
			if info.traits:find("H") then
				trunc_bits = 4
			end
			local trunc = 2 ^ trunc_bits
			if operand.value < 0 and operand.value >= -trunc / 2 then
				operand.value = operand.value + trunc
			end
			if operand.value >= trunc then
				operand.value = operand.value % trunc
				operand.token:blamef(printf.warn, "number truncated to %i bits", trunc_bits)
			end
			if named_op_index < imm_index then
				named_op_index = imm_index
			end
		end
		if named_op_index == 1 and not info.traits:find("P") then
			named_op_index = 2
		end
		if named_op_index == 2 and not info.traits:find("S") then
			named_op_index = 3
		end
		if named_op_index == 3 and not info.traits:find("T") then
			named_op_index = 4
		end
		named_ops[named_op_index] = operand
		named_op_index = named_op_index + 1
	end
	if info.traits:find("T") and not named_ops[3] and named_ops[2] then
		named_ops[3], named_ops[2] = named_ops[2], named_ops[3]
	end
	if info.traits:find("X") then
		if info.traits:find("T") and named_ops[3] and named_ops[3].type == "imm" then
			named_ops[3].value = xbit32.bxor(named_ops[3].value, 0xFFFF)
			if info.companion_add_one then
				named_ops[3].value = xbit32.band(named_ops[3].value + 1, 0xFFFF)
			end
			code = xbit32.bxor(code, info.companion_code_xor)
		else
			named_ops[3], named_ops[2] = named_ops[2], named_ops[3]
		end
	end

	local final_code = nop:clone():merge(code, 0)
	if imm_count > 1 then
		final_code = nil
	end
	if final_code and info.traits:find("P") then
		if named_ops[1] then
			final_code = final_code:merge(named_ops[1].value, 25)
		else
			final_code = nil
		end
	end
	if final_code and info.traits:find("S") then
		local shift = info.traits:find("J") and 25 or 20
		if named_ops[2] then
			final_code = final_code:merge(named_ops[2].value, shift)
		elseif info.traits:find("D") and named_ops[1] then
			final_code = final_code:merge(named_ops[1].value, shift)
		elseif info.traits:find("M") and named_ops[3] then
			if named_ops[3].type == "imm" then
				final_code = final_code:merge(entities["r0"].offset, shift)
			else
				final_code = final_code:merge(named_ops[3].value, shift)
			end
		elseif info.traits:find("E") then
			final_code = final_code:merge(entities["r0"].offset, shift)
		else
			final_code = nil
		end
	end
	if final_code and info.traits:find("T") then
		if named_ops[3] then
			if named_ops[3].type == "imm" then
				final_code = final_code:merge(named_ops[3].value, 0):merge(0x40000000, 0)
			else
				final_code = final_code:merge(named_ops[3].value, 0)
			end
		elseif info.traits:find("X") and info.traits:find("D") and named_ops[1] then
			final_code = final_code:merge(named_ops[1].value, 0)
		else
			final_code = nil
		end
	end
	if named_ops[4] then
		final_code = nil
	end

	if not final_code then
		local operands_repr = {}
		for _, ix_oper in ipairs(operands) do
			table.insert(operands_repr, ix_oper.type)
		end
		mnemonic_token:blamef(printf.err, "no variant of %s exists that takes '%s' operands", mnemonic_token.value, table.concat(operands_repr, ", "))
		return false
	end
	return true, { final_code }
end
for mnemonic in pairs(mnemonic_info) do
	mnemonics[mnemonic] = mnemonic_desc
end

local function flash(model, target, opcodes)
	local x, y = detect.cpu(model, target)
	if not x then
		return
	end

	local memory_rows_str, core_count_str = assert(model:match("^R3A(..)(..)$"))
	local memory_rows = tonumber(memory_rows_str)
	local core_count = tonumber(core_count_str)
	local space_available = memory_rows * 128
	if #opcodes >= space_available then
		printf.err("out of space; code takes %i cells, only have %i", #opcodes + 1, space_available)
		return
	end

	local row_size = 128
	local row_count = space_available / row_size
	for index = 0, space_available - 1 do
		local index_x = index % row_size
		local index_y = math.floor(index / row_size)
		local opcode = opcodes[index] and opcodes[index].dwords[1] or xbit32.bor(xbit32.band(xbit32.bxor(index_y, index_x), 0xFFFF), 0x00308000)
		if xbit32.band(opcode, 0x3FFFFFFF) == 0 then
			opcode = xbit32.bxor(opcode, 0x20000000)
		end
		sim.partProperty(sim.partID(x + index_x - 41, y + index_y - 13 - row_count - core_count * 6), "ctype", opcode)
	end
end

return {
	includes  = includes,
	dw_bits   = dw_bits,
	nop       = nop,
	entities  = entities,
	mnemonics = mnemonics,
	flash     = flash,
}

end

modules["tptasm.config"] = function()
local config = {
	max_depth = {
		include = 100,
		expansion = 100,
		eval = 100
	},
	reserved = {
		appendvararg = "_Appendvararg",
		defined      = "_Defined",
		dw           = "_Dw",
		identity     = "_Identity",
		labelcontext = "_Labelcontext",
		litmap       = "_Litmap",
		macrounique  = "_Macrounique",
		model        = "_Model",
		next         = "_Next",
		org          = "_Org",
		peerlabel    = "_Peerlabel",
		superlabel   = "_Superlabel",
		this         = "_This",
		vararg       = "_Vararg",
		varargsize   = "_Varargsize",
	},
	litmap = {}
}
for ix = 0, 127 do
	config.litmap[ix] = ix
end

return config

end

modules["tptasm.detect"] = function()
local modulepack = require("modulepack")
local printf     = require("tptasm.printf")
local xbit32     = require("tptasm.xbit32")

local function enumerate_standard(id)
	if  sim.partProperty(id, "ctype") == 0x1864A205
	and sim.partProperty(id, "type") == elem.DEFAULT_PT_QRTZ then
		local x, y = sim.partPosition(id)
		local dxdyprop = sim.partProperty(id, "tmp2")
		local dx   = xbit32.band(              dxdyprop,      0xF)
		local dy   = xbit32.band(xbit32.rshift(dxdyprop, 4),  0xF)
		local prop = xbit32.band(xbit32.rshift(dxdyprop, 8), 0x1F)
		if dx > 8 then
			dx = dx - 16
		end
		if dy > 8 then
			dy = dy - 16
		end
		if dx == 0 and dy == 0 then
			dx = 1
		end
		if math.abs(dx) > 1 or math.abs(dy) > 1 then -- * Garbage.
			dy = 0
			dx = 1
		end
		if prop == 0 then
			prop = sim.FIELD_CTYPE
		end
		local function prop_of(offs)
			local cid = sim.partID(x + offs * dx, y + offs * dy)
			return cid and sim.partProperty(cid, prop)
		end
		local id_target = prop_of(-1)
		if id_target then
			local offs = 0
			local id_model = ""
			local checksum = 0
			local name_intact = true
			while true do
				offs = offs + 1
				local ctype = prop_of(offs)
				if not ctype or ctype < 0 or ctype > 255 then
					name_intact = false
					break
				end
				if ctype == 0 then
					break
				end
				id_model = id_model .. string.char(ctype)
				checksum = checksum + ctype
			end
			if name_intact and prop_of(offs + 1) == checksum then
				coroutine.yield(x, y, id_model, id_target)
				return true
			end
		end
	end
	return false
end

local function enumerate_nope()
	-- * nothing, it's a placeholder and it just fails
end

local function match_property(id, name, value)
	return not value or sim.partProperty(id, name) == value
end

local function enumerate_legacy(model, conditions)
	return function(id)
		if  match_property(id, "type", conditions[1][3])
		and match_property(id, "ctype", conditions[1][4]) then
			local x, y = sim.partPosition(id)
			local ok = true
			for ix = 2, #conditions do
				local cid = sim.partID(x + conditions[ix][1], y + conditions[ix][2])
				if not cid
				or not match_property(cid, "type", conditions[ix][3])
				or not match_property(cid, "ctype", conditions[ix][4]) then
					ok = false
				end
			end
			if ok then
				coroutine.yield(x, y, model, 0)
				return true
			end
		end
		return false
	end
end

local enumerate_micro21 = enumerate_nope
local enumerate_maps = enumerate_nope
if _G.tpt then
	enumerate_micro21 = enumerate_legacy("MICRO21", {
		{  nil, nil, elem.DEFAULT_PT_DTEC, elem.DEFAULT_PT_DSTW },
		{ -181, 292, elem.DEFAULT_PT_DMND,                false },
		{  336, 201, elem.DEFAULT_PT_DMND,                false },
		{  394,  23, elem.DEFAULT_PT_BTRY,                false },
		{  384,  85, elem.DEFAULT_PT_BTRY,                false },
		{  -13, 177, elem.DEFAULT_PT_BTRY,                false },
		{ -179, 306, elem.DEFAULT_PT_PTCT,                false },
		{ -175, 306, elem.DEFAULT_PT_PTCT,                false },
		{ -178, 309, elem.DEFAULT_PT_PTCT,                false },
		{ -174, 309, elem.DEFAULT_PT_PTCT,                false },
	})
	enumerate_maps = enumerate_legacy("MAPS", {
		{  nil, nil, elem.DEFAULT_PT_SWCH, false },
		{  -28,  -2, elem.DEFAULT_PT_ARAY, false },
		{  137, -27, elem.DEFAULT_PT_DLAY, false },
		{   67, -11, elem.DEFAULT_PT_INST, false },
		{   49,  19, elem.DEFAULT_PT_FILT, false },
		{  -12,  34, elem.DEFAULT_PT_INWR, false },
		{   90,   3, elem.DEFAULT_PT_ARAY, false },
		{   94, -42, elem.DEFAULT_PT_INSL, false },
		{  124,  12, elem.DEFAULT_PT_SWCH, false },
		{  113,  11, elem.DEFAULT_PT_METL, false },
	})
end

local function enumerate_cpus()
	if not _G.tpt then
		printf.err("not running inside TPT, can't find target")
		return
	end
	for id in sim.parts() do
		local _ = enumerate_standard(id)
		       or enumerate_micro21(id)
		       or enumerate_maps(id)
	end
end

local function all_cpus()
	local co = coroutine.create(function()
		local rethrow = false
		modulepack.xpcall_wrap(function()
			enumerate_cpus()
		end, function(err)
			rethrow = true
		end)()
		if rethrow then
			error("rethrowing error from inside xpcall")
		end
	end)
	return function()
		if coroutine.status(co) ~= "dead" then
			local ok, x, y, id_model, id_target = coroutine.resume(co)
			if not ok then
				error(x)
			end
			return x, y, id_model, id_target
		end
	end
end

local function detect_filter(filter)
	local candidates = {}
	for x, y, model, id in all_cpus() do
		if filter(x, y, model, id) then
			table.insert(candidates, {
				x = x,
				y = y,
				model = model,
				id = id
			})
		end
	end
	if _G.tpt then
		local mx, my = sim.adjustCoords(tpt.mousex, tpt.mousey)
		for _, candidate in ipairs(candidates) do
			if candidate.x == mx and candidate.y == my then
				candidates = { candidate }
				break
			end
		end
	end
	if candidates[1] then
		return candidates[1]
	end
end

local function cpu(model_in, target_in)
	local candidate = detect_filter(function(x, y, model, id)
		return (not target_in or target_in == id   )
		   and (not  model_in or  model_in == model)
	end)
	if candidate then
		return candidate.x, candidate.y
	end
end

local function model(target_in)
	local candidate = detect_filter(function(x, y, model, id)
		return (not target_in or target_in == id)
	end)
	if candidate then
		return candidate.model
	end
end

local function make_anchor(model, dxstr, dystr, propname, leetid)
	if not _G.tpt then
		printf.err("not running inside TPT, can't spawn anchor")
		return
	end
	local prop = sim["FIELD_" .. tostring(propname or "ctype"):upper()]
	if not prop then
		printf.err("invalid property")
		return
	end
	local dx = tonumber(dxstr or "1")
	if dx ~= math.floor(dx) or dx >= 8 or dx < -8 then
		printf.err("invalid dx")
		return
	end
	local edx = dx
	if edx < 0 then
		edx = edx + 16
	end
	local dy = tonumber(dystr or "0")
	if dy ~= math.floor(dy) or dy >= 8 or dy < -8 then
		printf.err("invalid dy")
		return
	end
	local edy = dy
	if edy < 0 then
		edy = edy + 16
	end
	local x, y = sim.adjustCoords(tpt.mousex, tpt.mousey)
	local function spawn(offs, ty)
		local px = x + offs * dx
		local py = y + offs * dy
		local id = sim.partID(px, py) or sim.partCreate(-2, px, py, ty)
		return id
	end
	sim.partProperty(spawn(-1, elem.DEFAULT_PT_FILT), prop, leetid or 1337)
	local anchor = spawn(0, elem.DEFAULT_PT_QRTZ)
	sim.partProperty(anchor, "tmp2", edx + edy * 0x10 + prop * 0x100)
	sim.partProperty(anchor, "ctype", 0x1864A205)
	local checksum = 0
	for ix = 1, #model do
		local byte = model:byte(ix)
		sim.partProperty(spawn(ix, elem.DEFAULT_PT_FILT), prop, byte)
		checksum = checksum + byte
	end
	spawn(#model + 1, elem.DEFAULT_PT_FILT)
	sim.partProperty(spawn(#model + 2, elem.DEFAULT_PT_FILT), prop, checksum)
end

return {
	all_cpus = all_cpus,
	cpu = cpu,
	model = model,
	make_anchor = make_anchor,
}

end

modules["tptasm.emit"] = function()
local printf  = require("tptasm.printf")
local resolve = require("tptasm.resolve")

return function(architecture, to_emit, labels)
	local opcodes = {}
	do
		local max_pointer = 0
		for _, rec in pairs(to_emit) do
			local after_end = rec.offset + rec.length
			if max_pointer < after_end then
				max_pointer = after_end
			end
		end
		for ix = 0, max_pointer - 1 do
			opcodes[ix] = architecture.nop:clone()
		end
	end
	for offset, rec in pairs(to_emit) do
		if type(rec.emit) == "function" then
			local emission_ok = true
			for ix, ix_param in ipairs(rec.parameters) do
				local labels_ok, ix, err = resolve.label_offsets(false, ix_param, labels, rec)
				if labels_ok then
					local evals_ok, ix, jx, err = resolve.evaluations(false, ix_param, labels, rec)
					if evals_ok then
						local numbers_ok, ix, err = resolve.numbers(false, ix_param)
						if not numbers_ok then
							ix_param[ix]:blamef(printf.err, "invalid number: %s", err)
							emission_ok = false
						end
					else
						ix_param[ix].value[jx]:blamef(printf.err, "evaluation failed: %s", err)
						emission_ok = false
					end
				else
					ix_param[ix]:blamef(printf.err, "failed to resolve label: %s", err)
					emission_ok = false
				end
			end
			if emission_ok then
				local emitted
				emission_ok, emitted = rec.emit(rec.emitted_by, rec.parameters, offset)
				if emission_ok then
					for ix = 1, rec.length do
						opcodes[offset + ix - 1] = emitted[ix]
					end
				end
			end
			if not emission_ok then
				printf.err_called = true
			end

		elseif type(rec.emit) == "table" then
			for ix = 1, rec.length do
				opcodes[offset + ix - 1] = rec.emit[ix]
			end

		end
	end
	if printf.err_called then
		printf.failf("opcode emission stage failed, bailing")
	end

	return opcodes
end

end

modules["tptasm.evaluate"] = function()
local config = require("tptasm.config")
local xbit32 = require("tptasm.xbit32")

local operator_funcs = {
	[">="] = { params = { "number", "number" }, does = function(a, b) return (a >= b) and 1 or 0 end },
	["<="] = { params = { "number", "number" }, does = function(a, b) return (a <= b) and 1 or 0 end },
	[">" ] = { params = { "number", "number" }, does = function(a, b) return (a >  b) and 1 or 0 end },
	["<" ] = { params = { "number", "number" }, does = function(a, b) return (a <  b) and 1 or 0 end },
	["=="] = { params = { "number", "number" }, does = function(a, b) return (a == b) and 1 or 0 end },
	["!="] = { params = { "number", "number" }, does = function(a, b) return (a ~= b) and 1 or 0 end },
	["&&"] = { params = { "number", "number" }, does = function(a, b) return (a ~= 0 and b ~= 0) and 1 or 0 end },
	["||"] = { params = { "number", "number" }, does = function(a, b) return (a ~= 0 or  b ~= 0) and 1 or 0 end },
	["!" ] = { params = { "number"           }, does = function(a) return (a == 0) and 1 or 0 end },
	["~" ] = { params = { "number"           }, does = function(a) return xbit32.bxor(a, 0xFFFFFFFF) end },
	["<<"] = { params = { "number", "number" }, does = xbit32.lshift },
	[">>"] = { params = { "number", "number" }, does = xbit32.rshift },
	["-" ] = { params = { "number", "number" }, does =    xbit32.sub },
	["+" ] = { params = { "number", "number" }, does =    xbit32.add },
	["/" ] = { params = { "number", "number" }, does =    xbit32.div },
	["%" ] = { params = { "number", "number" }, does =    xbit32.mod },
	["*" ] = { params = { "number", "number" }, does =    xbit32.mul },
	["&" ] = { params = { "number", "number" }, does =   xbit32.band },
	["|" ] = { params = { "number", "number" }, does =    xbit32.bor },
	["^" ] = { params = { "number", "number" }, does =   xbit32.bxor },
	[config.reserved.defined] = { params = { "alias" }, does = function(a) return a and 1 or 0 end },
	[config.reserved.identity] = { params = { "number" }, does = function(a) return a end },
}
local operators = {}
for key in pairs(operator_funcs) do
	table.insert(operators, key)
end
table.sort(operators, function(a, b)
	return #a > #b
end)

local function evaluate_composite(composite)
	if composite.type == "number" then
		return composite.value
	end
	return composite.operator.does(function(ix)
		return evaluate_composite(composite.operands[ix])
	end)
end

return function(tokens, cursor, last, aliases)
	local stack = {}

	local function apply_operator(operator_name)
		local operator = operator_funcs[operator_name]
		if #stack < #operator.params then
			return false, cursor, ("operator takes %i operands, %i supplied"):format(#operator.params, #stack)
		end
		local max_depth = 0
		local operands = {}
		for ix = #stack - #operator.params + 1, #stack do
			if max_depth < stack[ix].depth then
				max_depth = stack[ix].depth
			end
			table.insert(operands, stack[ix])
			stack[ix] = nil
		end
		if max_depth > config.max_depth.eval then
			return false, cursor, "maximum evaluation depth reached"
		end
		for ix = 1, #operands do
			if operator.params[ix] == "number" then
				if operands[ix].type == "number" then
					operands[ix] = operands[ix].value
				elseif operands[ix].type == "alias" then
					local alias = operands[ix].value
					local ok, number
					if #alias == 1 then
						ok, number = alias[1]:parse_number()
					end
					if not ok then
						return false, operands[ix].position, ("operand %i does not expand to a number"):format(ix)
					end
					operands[ix] = number
				else
					return false, operands[ix].position, ("operand %i is %s, should be number"):format(ix, operands[ix].type)
				end
			elseif operator.params[ix] == "alias" then
				if operands[ix].type == "alias" then
					operands[ix] = operands[ix].value
				else
					return false, operands[ix].position, ("operand %i is %s, should be alias"):format(ix, operands[ix].type)
				end
			end
		end
		table.insert(stack, {
			type = "number",
			value = operator.does(unpack(operands)),
			position = cursor,
			depth = max_depth + 1
		})
		return true
	end

	while cursor <= last do
		if tokens[cursor]:number() then
			local ok, number = tokens[cursor]:parse_number()
			if not ok then
				return false, cursor, ("invalid number: %s"):format(number)
			end
			table.insert(stack, {
				type = "number",
				value = number,
				position = cursor,
				depth = 1
			})
			cursor = cursor + 1

		elseif tokens[cursor]:punctuator() then
			local found
			for _, known_operator in ipairs(operators) do
				local matches = true
				for pos, ch in known_operator:gmatch("()(.)") do
					local relative = cursor + pos - 1
					if (relative > last)
					or (pos < #known_operator and tokens[relative].whitespace_follows)
					or (not tokens[relative]:punctuator(ch)) then
						matches = false
						break
					end
				end
				if matches then
					found = known_operator
					break
				end
			end
			if not found then
				return false, cursor, "unknown operator"
			end
			local ok, pos, err = apply_operator(found)
			if not ok then
				return false, pos, err
			end
			cursor = cursor + #found

		elseif tokens[cursor]:identifier() and operator_funcs[tokens[cursor].value] then
			local ok, pos, err = apply_operator(tokens[cursor].value)
			if not ok then
				return false, pos, err
			end

		elseif tokens[cursor]:identifier() and aliases[tokens[cursor].value] then
			table.insert(stack, {
				type = "alias",
				value = aliases[tokens[cursor].value],
				position = cursor,
				depth = 1
			})
			cursor = cursor + 1

		else
			return false, cursor, "not a number, an identifier or an operator"

		end
	end

	local ok, pos, err = apply_operator(config.reserved.identity)
	if not ok then
		return false, pos, err
	end
	if #stack > 1 then
		return false, stack[#stack - 1].position, "excess value"
	end
	if #stack < 1 then
		return false, 1, "no value"
	end
	return true, stack[1].value
end

end

modules["tptasm.opcode"] = function()
local xbit32 = require("tptasm.xbit32")

local opcode_i = {}
local opcode_mt = { __index = opcode_i }

function opcode_i:clone()
	local dwords = {}
	for ix, ix_dword in ipairs(self.dwords) do
		dwords[ix] = ix_dword
	end
	return setmetatable({
		dwords = dwords
	}, opcode_mt)
end

function opcode_i:dump()
	return ("%08X "):rep(#self.dwords):format(unpack(self.dwords))
end

function opcode_i:has(shift)
	return math.floor(self.dwords[math.floor(shift / 32) + 1] / 2 ^ (shift % 32)) % 2 == 1
end

function opcode_i:merge(thing, shift)
	if type(thing) == "table" then
		for ix, ix_dword in ipairs(thing.dwords) do
			self:merge(ix_dword, (ix - 1) * 32 + shift)
		end
	else
		local offs = 1
		while shift >= 32 do
			offs = offs + 1
			shift = shift - 32
		end
		self.dwords[offs] = xbit32.bor(self.dwords[offs], thing % 2 ^ (32 - shift) * 2 ^ shift)
		thing = math.floor(thing / 2 ^ (32 - shift))
		for ix = offs + 1, #self.dwords do
			if thing == 0 then
				break
			end
			self.dwords[ix] = xbit32.bor(self.dwords[ix], thing % 0x100000000)
			thing = math.floor(thing / 0x100000000)
		end
	end
	return self
end

local function make(size)
	local dwords = {}
	for ix = 1, math.ceil(size / 32) do
		dwords[ix] = 0
	end
	return setmetatable({
		dwords = dwords
	}, opcode_mt)
end

return {
	make = make,
}

end

modules["tptasm.preprocess"] = function()
local printf   = require("tptasm.printf")
local config   = require("tptasm.config")
local evaluate = require("tptasm.evaluate")
local resolve  = require("tptasm.resolve")
local tokenise = require("tptasm.tokenise")
local utility  = require("tptasm.utility")

local source_line_i = {}
local source_line_mt = { __index = source_line_i }

function source_line_i:dump_itop()
	local included_from = self.itop
	while included_from do
		printf.info("  included from %s:%i", included_from.path, included_from.line)
		included_from = included_from.next
	end
end

function source_line_i:blamef(report, format, ...)
	report("%s:%i: " .. format, self.path, self.line, ...)
	self:dump_itop()
end

function source_line_i:blamef_after(report, token, format, ...)
	report("%s:%i:%i " .. format, self.path, self.line, token.soffs + #token.value, ...)
	self:dump_itop()
end

return function(architecture, path)
	local macro_invocation_counter = 0
	local lines = {}
	local include_top = false
	local include_depth = 0

	local function preprocess_fail()
		printf.failf("preprocessing stage failed, bailing")
	end

	local aliases = {}
	local function expand_aliases(tokens, first, last, depth)
		local expanded = {}
		for ix = first, last do
			local alias = tokens[ix]:identifier() and aliases[tokens[ix].value]
			if alias then
				if depth > config.max_depth.expansion then
					tokens[ix]:blamef(printf.err, "maximum expansion depth reached while expanding alias '%s'", tokens[ix].value)
					preprocess_fail()
				end
				for _, token in ipairs(expand_aliases(alias, 1, #alias, depth + 1)) do
					table.insert(expanded, token:expand_by(tokens[ix]))
				end
			else
				table.insert(expanded, tokens[ix])
			end
		end
		return expanded
	end
	local function define(identifier, tokens, first, last)
		if aliases[identifier.value] then
			identifier:blamef(printf.err, "alias '%s' is defined", identifier.value)
			preprocess_fail()
		end
		local alias = {}
		for ix = first, last do
			table.insert(alias, tokens[ix])
		end
		aliases[identifier.value] = alias
	end
	local function undef(identifier)
		if not aliases[identifier.value] then
			identifier:blamef(printf.err, "alias '%s' is not defined", identifier.value)
			preprocess_fail()
		end
		aliases[identifier.value] = nil
	end

	local macros = {}
	local defining_macro = false
	local function expand_macro(tokens, depth)
		local expanded = expand_aliases(tokens, 1, #tokens, depth + 1)
		local macro = expanded[1]:identifier() and macros[expanded[1].value]
		if macro then
			if depth > config.max_depth.expansion then
				expanded[1]:blamef(printf.err, "maximum expansion depth reached while expanding macro '%s'", expanded[1].value)
				preprocess_fail()
			end
			local expanded_lines = {}
			local parameters_passed = {}
			local parameter_list = resolve.parameters(expanded[1], expanded, 2, #expanded)
			for ix, ix_param in ipairs(parameter_list) do
				parameters_passed[macro.params[ix] or false] = ix_param
			end
			if macro.vararg then
				if #macro.params > #parameter_list then
					expanded[1]:blamef(printf.err, "macro '%s' invoked with %i parameters, expects at least %i", expanded[1].value, #parameter_list, #macro.params)
					preprocess_fail()
				end
			else
				if #macro.params ~= #parameter_list then
					expanded[1]:blamef(printf.err, "macro '%s' invoked with %i parameters, expects %i", expanded[1].value, #parameter_list, #macro.params)
					preprocess_fail()
				end
			end
			macro_invocation_counter = macro_invocation_counter + 1
			parameters_passed[config.reserved.macrounique] = { expanded[1]:point({
				type = "identifier",
				value = ("_%i_"):format(macro_invocation_counter)
			}) }
			if macro.vararg then
				local vararg_param = {}
				local appendvararg_param = {}
				if #parameter_list > #macro.params then
					table.insert(appendvararg_param, expanded[1]:point({
						type = "punctuator",
						value = ","
					}))
				end
				for ix = #macro.params + 1, #parameter_list do
					for _, ix_token in ipairs(parameter_list[ix]) do
						table.insert(vararg_param, ix_token)
						table.insert(appendvararg_param, ix_token)
					end
					if ix ~= #parameter_list then
						table.insert(vararg_param, parameter_list[ix + 1].before)
						table.insert(appendvararg_param, parameter_list[ix + 1].before)
					end
				end
				parameters_passed[config.reserved.vararg] = vararg_param
				parameters_passed[config.reserved.appendvararg] = appendvararg_param
				parameters_passed[config.reserved.varargsize] = { expanded[1]:point({
					type = "number",
					value = tostring(#parameter_list - #macro.params)
				}) }
			end
			local old_aliases = {}
			for param, value in pairs(parameters_passed) do
				old_aliases[param] = aliases[param]
				aliases[param] = value
			end
			for _, line in ipairs(macro) do
				for _, expanded_line in ipairs(expand_macro(line.tokens, depth + 1)) do
					local cloned_line = {}
					for _, token in ipairs(expanded_line) do
						table.insert(cloned_line, token:expand_by(expanded[1]))
					end
					table.insert(expanded_lines, cloned_line)
				end
			end
			for param, value in pairs(parameters_passed) do
				aliases[param] = old_aliases[param]
			end
			return expanded_lines
		else
			return { expanded }
		end
	end
	local function macro(identifier, tokens, first, last)
		if macros[identifier.value] then
			identifier:blamef(printf.err, "macro '%s' is defined", identifier.value)
			preprocess_fail()
		end
		local params = {}
		local params_assoc = {}
		local vararg = false
		for ix = first, last, 2 do
			if  ix + 2 == last
			and tokens[ix    ]:punctuator(".") and not tokens[ix    ].whitespace_follows
			and tokens[ix + 1]:punctuator(".") and not tokens[ix + 1].whitespace_follows
			and tokens[ix + 2]:punctuator(".") then
				vararg = true
				break
			end
			if not tokens[ix]:identifier() then
				tokens[ix]:blamef(printf.err, "expected parameter name")
				preprocess_fail()
			end
			if params_assoc[tokens[ix].value] then
				tokens[ix]:blamef(printf.err, "duplicate parameter")
				preprocess_fail()
			end
			params_assoc[tokens[ix].value] = true
			table.insert(params, tokens[ix].value)
			if ix == last then
				break
			end
			if not tokens[ix + 1]:punctuator(",") then
				tokens[ix + 1]:blamef(printf.err, "expected comma")
				preprocess_fail()
			end
		end
		defining_macro = {
			params = params,
			name = identifier.value,
			vararg = vararg
		}
	end
	local function endmacro()
		macros[defining_macro.name] = defining_macro
		defining_macro = false
	end
	local function unmacro(identifier)
		if not macros[identifier.value] then
			identifier:blamef(printf.err, "macro '%s' is not defined", identifier.value)
			preprocess_fail()
		end
		macros[identifier.value] = nil
	end

	local condition_stack = { {
		condition = true,
		seen_else = false,
		been_true = true,
		opened_by = false
	} }

	local function include(base_path, relative_path, lines, req)
		if include_depth > config.max_depth.include then
			req:blamef(printf.err, "maximum include depth reached while including '%s'", relative_path)
			preprocess_fail()
		end
		local path = relative_path
		local content = architecture.includes[relative_path]
		if not content then
			path = base_path and utility.resolve_relative(base_path, relative_path) or relative_path
			local handle = io.open(path, "r")
			if not handle then
				req:blamef(printf.err, "failed to open '%s' for reading", path)
				preprocess_fail()
			end
			content = handle:read("*a")
			handle:close()
		end

		local line_number = 0
		for line in (content .. "\n"):gmatch("([^\n]*)\n") do
			line_number = line_number + 1
			local sline = setmetatable({
				path = path,
				line = line_number,
				itop = include_top,
				str = line
			}, source_line_mt)
			local ok, tokens, err = tokenise(sline)
			if not ok then
				printf.err("%s:%i:%i: %s", sline.path, sline.line, tokens, err)
				preprocess_fail()
			end
			if #tokens >= 1 and tokens[1]:punctuator("%") then
				if #tokens >= 2 and tokens[2]:identifier() then

					if tokens[2].value == "include" then
						if condition_stack[#condition_stack].condition then
							if #tokens < 3 then
								sline:blamef_after(printf.err, tokens[2], "expected path")
								preprocess_fail()
							elseif not tokens[3]:stringlit() then
								tokens[3]:blamef(printf.err, "expected path")
								preprocess_fail()
							end
							if #tokens > 3 then
								tokens[4]:blamef(printf.err, "expected end of line")
								preprocess_fail()
							end
							local relative_path = tokens[3].value:gsub("^\"(.*)\"$", "%1")
							include_top = {
								path = path,
								line = line_number,
								next = include_top
							}
							include_depth = include_depth + 1
							include(path, relative_path, lines, sline)
							include_depth = include_depth - 1
							include_top = include_top.next
						end

					elseif tokens[2].value == "warning" or tokens[2].value == "error" then
						if condition_stack[#condition_stack].condition then
							if #tokens < 3 then
								sline:blamef_after(printf.err, tokens[2], "expected message")
								preprocess_fail()
							elseif not tokens[3]:stringlit() then
								tokens[3]:blamef(printf.err, "expected message")
								preprocess_fail()
							end
							if #tokens > 3 then
								tokens[4]:blamef(printf.err, "expected end of line")
								preprocess_fail()
							end
							local err = tokens[3].value:gsub("^\"(.*)\"$", "%1")
							if tokens[2].value == "error" then
								tokens[2]:blamef(printf.err, "%%error: %s", err)
								preprocess_fail()
							else
								tokens[2]:blamef(printf.warn, "%%warning: %s", err)
							end
						end

					elseif tokens[2].value == "eval" then
						if condition_stack[#condition_stack].condition then
							if #tokens < 3 then
								sline:blamef_after(printf.err, tokens[2], "expected alias name")
								preprocess_fail()
							elseif not tokens[3]:identifier() then
								tokens[3]:blamef(printf.err, "expected alias name")
								preprocess_fail()
							end
							local ok, result, err = evaluate(tokens, 4, #tokens, aliases)
							if not ok then
								tokens[result]:blamef(printf.err, "evaluation failed: %s", err)
								preprocess_fail()
							end
							define(tokens[3], { tokens[3]:point({
								type = "number",
								value = tostring(result)
							}) }, 1, 1)
						end

					elseif tokens[2].value == "define" then
						if condition_stack[#condition_stack].condition then
							if #tokens < 3 then
								sline:blamef_after(printf.err, tokens[2], "expected alias name")
								preprocess_fail()
							elseif not tokens[3]:identifier() then
								tokens[3]:blamef(printf.err, "expected alias name")
								preprocess_fail()
							end
							define(tokens[3], tokens, 4, #tokens)
						end

					elseif tokens[2].value == "undef" then
						if condition_stack[#condition_stack].condition then
							if #tokens < 3 then
								sline:blamef_after(printf.err, tokens[2], "expected alias name")
								preprocess_fail()
							elseif not tokens[3]:identifier() then
								tokens[3]:blamef(printf.err, "expected alias name")
								preprocess_fail()
							end
							if #tokens > 3 then
								tokens[4]:blamef(printf.err, "expected end of line")
								preprocess_fail()
							end
							undef(tokens[3])
						end

					elseif tokens[2].value == "if" then
						local ok, result, err = evaluate(tokens, 3, #tokens, aliases)
						if not ok then
							tokens[result]:blamef(printf.err, "evaluation failed: %s", err)
							preprocess_fail()
						end
						local evals_to_true = result ~= 0
						condition_stack[#condition_stack + 1] = {
							condition = evals_to_true,
							seen_else = false,
							been_true = evals_to_true,
							opened_by = tokens[2]
						}

					elseif tokens[2].value == "ifdef" then
						if #tokens < 3 then
							sline:blamef_after(printf.err, tokens[2], "expected alias name")
							preprocess_fail()
						elseif not tokens[3]:identifier() then
							tokens[3]:blamef(printf.err, "expected alias name")
							preprocess_fail()
						end
						if #tokens > 3 then
							tokens[4]:blamef(printf.err, "expected end of line")
							preprocess_fail()
						end
						local evals_to_true = aliases[tokens[3].value] and true
						condition_stack[#condition_stack + 1] = {
							condition = evals_to_true,
							seen_else = false,
							been_true = evals_to_true,
							opened_by = tokens[2]
						}

					elseif tokens[2].value == "ifndef" then
						if #tokens < 3 then
							sline:blamef_after(printf.err, tokens[2], "expected alias name")
							preprocess_fail()
						elseif not tokens[3]:identifier() then
							tokens[3]:blamef(printf.err, "expected alias name")
							preprocess_fail()
						end
						if #tokens > 3 then
							tokens[4]:blamef(printf.err, "expected end of line")
							preprocess_fail()
						end
						local evals_to_true = not aliases[tokens[3].value] and true
						condition_stack[#condition_stack + 1] = {
							condition = evals_to_true,
							seen_else = false,
							been_true = evals_to_true,
							opened_by = tokens[2]
						}

					elseif tokens[2].value == "else" then
						if #condition_stack == 1 then
							tokens[2]:blamef(printf.err, "unpaired %%else")
							preprocess_fail()
						end
						if condition_stack[#condition_stack].seen_else then
							tokens[2]:blamef(printf.err, "%%else after %%else")
							preprocess_fail()
						end
						condition_stack[#condition_stack].seen_else = true
						if condition_stack[#condition_stack].been_true then
							condition_stack[#condition_stack].condition = false
						else
							condition_stack[#condition_stack].condition = true
							condition_stack[#condition_stack].been_true = true
						end

					elseif tokens[2].value == "elif" then
						if #tokens > 2 then
							tokens[3]:blamef(printf.err, "expected end of line")
							preprocess_fail()
						end
						if #condition_stack == 1 then
							tokens[2]:blamef(printf.err, "unpaired %%elif")
							preprocess_fail()
						end
						if condition_stack[#condition_stack].seen_else then
							tokens[2]:blamef(printf.err, "%%elif after %%else")
							preprocess_fail()
						end
						if condition_stack[#condition_stack].been_true then
							condition_stack[#condition_stack].condition = false
						else
							local ok, result, err = evaluate(tokens, 3, #tokens, aliases)
							if not ok then
								tokens[result]:blamef(printf.err, "evaluation failed: %s", err)
								preprocess_fail()
							end
							local evals_to_true = result ~= 0
							condition_stack[#condition_stack].condition = evals_to_true
							condition_stack[#condition_stack].been_true = evals_to_true
						end

					elseif tokens[2].value == "endif" then
						if #tokens > 2 then
							tokens[3]:blamef(printf.err, "expected end of line")
							preprocess_fail()
						end
						if #condition_stack == 1 then
							tokens[2]:blamef(printf.err, "unpaired %%endif")
							preprocess_fail()
						end
						condition_stack[#condition_stack] = nil

					elseif tokens[2].value == "macro" then
						if condition_stack[#condition_stack].condition then
							if #tokens < 3 then
								sline:blamef_after(printf.err, tokens[2], "expected macro name")
								preprocess_fail()
							elseif not tokens[3]:identifier() then
								tokens[3]:blamef(printf.err, "expected macro name")
								preprocess_fail()
							end
							if defining_macro then
								tokens[2]:blamef(printf.err, "%%macro after %%macro")
								preprocess_fail()
							end
							macro(tokens[3], tokens, 4, #tokens)
						end
						
					elseif tokens[2].value == "endmacro" then
						if condition_stack[#condition_stack].condition then
							if #tokens > 2 then
								tokens[3]:blamef(printf.err, "expected end of line")
								preprocess_fail()
							end
							if not defining_macro then
								tokens[2]:blamef(printf.err, "unpaired %%endmacro")
								preprocess_fail()
							end
							endmacro()
						end

					elseif tokens[2].value == "unmacro" then
						if condition_stack[#condition_stack].condition then
							if #tokens < 3 then
								sline:blamef_after(printf.err, tokens[2], "expected macro name")
								preprocess_fail()
							elseif not tokens[3]:identifier() then
								tokens[3]:blamef(printf.err, "expected macro name")
								preprocess_fail()
							end
							if #tokens > 3 then
								tokens[4]:blamef(printf.err, "expected end of line")
								preprocess_fail()
							end
							unmacro(tokens[3])
						end

					else
						tokens[2]:blamef(printf.err, "unknown preprocessing directive")
						preprocess_fail()

					end
				end
			else
				if condition_stack[#condition_stack].condition and #tokens > 0 then
					if defining_macro then
						table.insert(defining_macro, {
							sline = sline,
							tokens = tokens
						})
					else
						for _, line in ipairs(expand_macro(tokens, 0)) do
							table.insert(lines, line)
						end
					end
				end
			end
		end
	end

	include(false, path, lines, { blamef = function(self, report, ...)
		report(...)
	end })
	if #condition_stack > 1 then
		condition_stack[#condition_stack].opened_by:blamef(printf.err, "unfinished conditional block")
		preprocess_fail()
	end

	return lines
end

end

modules["tptasm.printf"] = function()
local printf
printf = setmetatable({
	print      = print,
	print_old  = print,
	log_handle = false,
	colour     = false,
	err_called = false,
	silent     = false
}, { __call = function(self, ...)
	if not printf.silent then
		printf.print(string.format(...))
	end
end })

function printf.debug(from, first, ...)
	local things = { tostring(first) }
	for ix_thing, thing in ipairs({ ... }) do
		table.insert(things, tostring(thing))
	end
	printf((printf.colour and "[tptasm] " or "[tptasm] [DD] ") .. "[%s] %s", from, table.concat(things, "\t"))
end

function printf.info(format, ...)
	printf((printf.colour and "\008t[tptasm]\008w " or "[tptasm] [II] ") .. format, ...)
end

function printf.warn(format, ...)
	printf((printf.colour and "\008o[tptasm]\008w " or "[tptasm] [WW] ") .. format, ...)
end

function printf.err(format, ...)
	printf((printf.colour and "\008l[tptasm]\008w " or "[tptasm] [EE] ") .. format, ...)
	printf.err_called = true
end

function printf.redirect(log_path)
	local handle = type(log_path) == "string" and io.open(log_path, "w") or log_path
	if handle then
		printf.log_path = log_path
		printf.log_handle = handle
		printf.info("redirecting log to '%s'", tostring(log_path))
		printf.print = function(str)
			printf.log_handle:write(str .. "\n")
		end
	else
		printf.warn("failed to open '%s' for writing, log not redirected", tostring(printf.log_path))
	end
	printf.update_colour()
end

function printf.unredirect()
	if printf.log_handle then
		if type(printf.log_path) == "string" then
			printf.log_handle:close()
		end
		printf.log_handle = false
		printf.print = printf.print_old
		printf.info("undoing redirection of log to '%s'", tostring(printf.log_path))
	end
	printf.update_colour()
end

function printf.update_colour()
	printf.colour = _G.tpt and not printf.log_handle
end

function printf.init()
	printf.update_colour()
	printf.err_called = false
	printf.silent = false
end

function printf.failf(...)
	printf.err(...)
	error(printf.failf)
end

return printf

end

modules["tptasm.resolve"] = function()
local printf   = require("tptasm.printf")
local config   = require("tptasm.config")
local evaluate = require("tptasm.evaluate")
local utility  = require("tptasm.utility")
local xbit32   = require("tptasm.xbit32")

local function parameters(before, expanded, first, last)
	local parameters = {}
	local parameter_buffer = {}
	local parameter_cursor = 0
	local last_comma = before
	local function flush_parameter()
		parameters[parameter_cursor] = parameter_buffer
		parameters[parameter_cursor].before = last_comma
		parameter_buffer = {}
	end
	if first <= last then
		parameter_cursor = 1
		for ix = first, last do
			if expanded[ix]:punctuator(",") then
				flush_parameter()
				last_comma = expanded[ix]
				parameter_cursor = parameter_cursor + 1
			else
				table.insert(parameter_buffer, expanded[ix])
			end
		end
		flush_parameter()
	end
	return parameters
end

local function numbers(for_length_calc, tokens)
	for ix, ix_token in ipairs(tokens) do
		if ix_token:number() then
			if not for_length_calc then
				local ok, number = ix_token:parse_number()
				if not ok then
					return false, ix, number
				end
				ix_token.parsed = number
			end
		elseif ix_token:charlit() then
			if not for_length_calc then
				local number = 0
				for first, last, code_point in utility.utf8_each(ix_token.value:gsub("^'(.*)'$", "%1")) do
					local mapped = config.litmap[code_point]
					if not mapped then
						ix_token:blamef(printf.warn, "code point %i at offset [%i, %i] not mapped, defaulting to 0", code_point, first, last)
						mapped = 0
					end
					number = xbit32.add(xbit32.lshift(number, 8), mapped)
				end
				ix_token.type = "number"
				ix_token.value = tostring(number)
				ix_token.parsed = number
			end
		end
	end
	return true
end

local function label_offsets(for_length_calc, tokens, labels, emit_rec)
	for ix, ix_token in ipairs(tokens) do
		if ix_token:identifier(config.reserved.this) then
			if for_length_calc then
				ix_token.type = "number"
				ix_token.value = "dummy"
			elseif emit_rec then
				ix_token.type = "number"
				ix_token.value = tostring(emit_rec.offset)
			end
		elseif ix_token:identifier(config.reserved.next) then
			if for_length_calc then
				ix_token.type = "number"
				ix_token.value = "dummy"
			elseif emit_rec then
				ix_token.type = "number"
				ix_token.value = tostring(emit_rec.offset + emit_rec.length)
			end
		elseif ix_token:is("label") then
			if for_length_calc then
				ix_token.type = "number"
				ix_token.value = "dummy"
			else
				local offs = labels[ix_token.value]
				if not offs then
					return false, ix, ix_token.value
				end
				ix_token.type = "number"
				ix_token.value = offs
			end
		end
	end
	return true
end

local function evaluations(for_length_calc, tokens, labels, emit_rec)
	for ix, ix_token in ipairs(tokens) do
		if ix_token:is("evaluation") then
			if for_length_calc then
				ix_token.type = "number"
				ix_token.value = "dummy"
			else
				local labels_ok, jx, err = label_offsets(for_length_calc, ix_token.value, labels, emit_rec)
				if labels_ok then
					local ok, result, err = evaluate(ix_token.value, 1, #ix_token.value, {})
					if ok then
						ix_token.type = "number"
						ix_token.value = tostring(result)
					else
						return false, ix, result, err
					end
				else
					return false, ix, jx, err
				end
			end
		end
	end
	return true
end


local function instructions(architecture, lines)
	local label_context = {}
	local output_pointer = 0
	local to_emit = {}
	local labels = {}
	local model_restrictions = {}

	local hooks = {}
	hooks[config.reserved.org] = function(hook_token, parameters)
		if #parameters < 1 then
			hook_token:blamef_after(printf.err, "expected origin")
			return false
		end
		if #parameters > 1 then
			parameters[1][#parameters[1]]:blamef_after(printf.err, "excess parameters")
			return false
		end
		local org_pack = parameters[1]
		if #org_pack > 1 then
			org_pack[2]:blamef(printf.err, "excess tokens")
			return false
		end
		local org = org_pack[1]
		if not org:is("number") then
			org:blamef(printf.err, "not a number")
			return false
		end
		output_pointer = org.parsed
		return true
	end
	hooks[config.reserved.dw] = function(hook_token, parameters)
		-- * TODO: allow higher shifts, currently dw constants are truncated
		--         to 32 bits. not sure how to get around this
		for _, ix_param in ipairs(parameters) do
			if #ix_param < 1 then
				ix_param.before:blamef_after(printf.err, "no tokens")
				return false
			end
			if #ix_param == 1 and ix_param[1]:stringlit() then
				local values = {}
				for first, last, code_point in utility.utf8_each(ix_param[1].value:gsub("^\"(.*)\"$", "%1")) do
					local mapped = config.litmap[code_point]
					if not mapped then
						ix_param[1]:blamef(printf.warn, "code point %i at offset [%i, %i] not mapped, defaulting to 0", code_point, first, last)
						mapped = 0
					end
					table.insert(values, architecture.nop:clone():merge(mapped, 0))
				end
				to_emit[output_pointer] = {
					emit = values,
					length = #values,
					emitted_by = ix_param[1],
					offset = output_pointer
				}
				to_emit[output_pointer].head = to_emit[output_pointer]
				output_pointer = output_pointer + #values
			else
				to_emit[output_pointer] = {
					emit = function()
						if #ix_param > 1 then
							ix_param[2]:blamef(printf.err, "excess tokens")
							return false
						end
						if not ix_param[1]:number() then
							ix_param[1]:blamef(printf.err, "expected string literal or number")
							return false
						end
						local number = ix_param[1].parsed
						if number >= 2 ^ architecture.dw_bits then
							number = number % 2 ^ architecture.dw_bits
							ix_param[1]:blamef(printf.warn, "number truncated to %i bits", architecture.dw_bits)
						end
						return true, { architecture.nop:clone():merge(number, 0) }
					end,
					parameters = { ix_param },
					length = 1,
					emitted_by = ix_param[1],
					offset = output_pointer
				}
				to_emit[output_pointer].head = to_emit[output_pointer]
				output_pointer = output_pointer + 1
			end
		end
		return true
	end
	hooks[config.reserved.litmap] = function(hook_token, parameters)
		if #parameters == 0 then
			hook_token:blamef_after(printf.err, "expected base")
			return false
		end
		if #parameters[1] < 1 then
			parameters[1]:blamef_after(printf.err, "no tokens")
			return false
		end
		if #parameters[1] > 1 then
			parameters[1][2]:blamef(printf.err, "excess tokens")
			return false
		end
		if not parameters[1][1]:number() then
			parameters[1][1]:blamef(printf.err, "not a number")
			return false
		end
		if #parameters == 1 then
			parameters[1][1]:blamef_after(printf.err, "expected map")
			return false
		end
		if #parameters[2] < 1 then
			parameters[2]:blamef_after(printf.err, "no tokens")
			return false
		end
		if #parameters[2] > 1 then
			parameters[2][2]:blamef(printf.err, "excess tokens")
			return false
		end
		if not parameters[2][1]:stringlit() then
			parameters[2][1]:blamef(printf.err, "not a string literal")
			return false
		end
		local base = parameters[1][1].parsed
		local map = parameters[2][1].value:gsub("^\"(.*)\"$", "%1")
		local counter = 0
		for first, last, code_point in utility.utf8_each(map) do
			config.litmap[code_point] = base + counter
			counter = counter + 1
		end
		return true
	end
	hooks[config.reserved.model] = function(hook_token, parameters)
		if #parameters < 1 then
			hook_token:blamef_after(printf.err, "expected models")
			return false
		end
		for _, model_pack in ipairs(parameters) do
			if #model_pack < 1 then
				model_pack[2].before:blamef_after(printf.err, "no tokens")
				return false
			elseif #model_pack > 1 then
				model_pack[2]:blamef(printf.err, "excess tokens")
				return false
			end
			local model = model_pack[1]
			if not model:is("stringlit") then
				model:blamef(printf.err, "not a string literal")
				return false
			end
			local restrictions = model_restrictions[model.sline.path]
			if not restrictions then
				restrictions = {}
				model_restrictions[model.sline.path] = restrictions
			end
			table.insert(restrictions, model)
		end
		return true
	end

	local reserved_set = {}
	for _, item in pairs(config.reserved) do
		reserved_set[item] = true
	end
	local function known_identifiers(name)
		return (architecture.entities[name]
		     or architecture.mnemonics[name]
		     or hooks[name]
		     or reserved_set[name]) and true
	end

	for _, tokens in ipairs(lines) do
		local line_failed = false

		if not line_failed then
			local cursor = #tokens
			while cursor >= 1 do
				if tokens[cursor]:stringlit() then
					while cursor > 1 and tokens[cursor - 1]:stringlit() do
						tokens[cursor - 1] = tokens[cursor - 1]:point({
							type = "stringlit",
							value = tokens[cursor - 1].value .. tokens[cursor].value
						})
						table.remove(tokens, cursor)
						cursor = cursor - 1
					end

				elseif tokens[cursor]:charlit() then
					while cursor > 1 and tokens[cursor - 1]:charlit() do
						tokens[cursor - 1] = tokens[cursor - 1]:point({
							type = "charlit",
							value = tokens[cursor - 1].value .. tokens[cursor].value
						})
						table.remove(tokens, cursor)
						cursor = cursor - 1
					end

				elseif tokens[cursor]:identifier() and architecture.entities[tokens[cursor].value] then
					tokens[cursor] = tokens[cursor]:point({
						type = "entity",
						value = tokens[cursor].value,
						entity = architecture.entities[tokens[cursor].value]
					})

				elseif tokens[cursor]:identifier() and architecture.mnemonics[tokens[cursor].value] then
					tokens[cursor] = tokens[cursor]:point({
						type = "mnemonic",
						value = tokens[cursor].value,
						mnemonic = architecture.mnemonics[tokens[cursor].value]
					})

				elseif tokens[cursor]:identifier() and hooks[tokens[cursor].value] then
					tokens[cursor] = tokens[cursor]:point({
						type = "hook",
						value = tokens[cursor].value,
						hook = hooks[tokens[cursor].value]
					})

				elseif (tokens[cursor]:identifier() and not known_identifiers(tokens[cursor].value)) or
					   (tokens[cursor]:identifier(config.reserved.labelcontext)) then
					while cursor > 1 do
						if tokens[cursor - 1]:identifier() and not known_identifiers(tokens[cursor - 1].value) then
							tokens[cursor - 1] = tokens[cursor - 1]:point({
								type = "identifier",
								value = tokens[cursor - 1].value .. tokens[cursor].value
							})
							table.remove(tokens, cursor)
							cursor = cursor - 1

						elseif tokens[cursor - 1]:identifier(config.reserved.peerlabel) then
							if #label_context < 1 then
								tokens[cursor - 1]:blamef(printf.err, "peer-label reference in level %i context", #label_context - 1)
								line_failed = true
								break
							end
							tokens[cursor - 1] = tokens[cursor - 1]:point({
								type = "identifier",
								value = ("."):rep(#label_context - 1) .. tokens[cursor].value
							})
							table.remove(tokens, cursor)
							cursor = cursor - 1

						elseif tokens[cursor - 1]:identifier(config.reserved.superlabel) then
							if #label_context < 2 then
								tokens[cursor - 1]:blamef(printf.err, "super-label reference in level %i context", #label_context - 1)
								line_failed = true
								break
							end
							tokens[cursor - 1] = tokens[cursor - 1]:point({
								type = "identifier",
								value = ("."):rep(#label_context - 2) .. tokens[cursor].value
							})
							table.remove(tokens, cursor)
							cursor = cursor - 1

						elseif tokens[cursor - 1]:punctuator(".") then
							tokens[cursor - 1] = tokens[cursor - 1]:point({
								type = "identifier",
								value = "." .. tokens[cursor].value
							})
							table.remove(tokens, cursor)
							cursor = cursor - 1

						else
							break

						end
					end

					if not line_failed then
						local dots, rest = tokens[cursor].value:match("^(%.*)(.+)$")
						local level = #dots
						if level > #label_context then
							tokens[cursor]:blamef(printf.err, "level %i label declaration without preceding level %i label declaration", level, level - 1)
							line_failed = true
							break
						else
							local name_tbl = {}
							for ix = 1, level do
								table.insert(name_tbl, label_context[ix])
							end
							table.insert(name_tbl, rest)
							tokens[cursor] = tokens[cursor]:point({
								type = "label",
								value = table.concat(name_tbl, "."),
								ignore = rest == config.reserved.labelcontext,
								level = level,
								rest = rest
							})
						end
					end

				end
				cursor = cursor - 1
			end
		end

		if not line_failed then
			local cursor = 1
			while cursor <= #tokens do
				if tokens[cursor]:punctuator("{") then
					local brace_end = cursor + 1
					local last
					while brace_end <= #tokens do
						if tokens[brace_end]:punctuator("}") then
							last = brace_end
							break
						end
						brace_end = brace_end + 1
					end
					if not last then
						tokens[cursor]:blamef(printf.err, "unfinished evaluation block")
						line_failed = true
						break
					end
					local eval_tokens = {}
					for ix = cursor + 1, last - 1 do
						table.insert(eval_tokens, tokens[ix])
					end
					for _ = cursor + 1, last do
						table.remove(tokens, cursor + 1)
					end
					tokens[cursor].type = "evaluation"
					tokens[cursor].value = eval_tokens
				end
				cursor = cursor + 1
			end
		end

		if not line_failed then
			while #tokens >= 2 and tokens[1]:is("label") and tokens[2]:punctuator(":") do
				if tokens[1].ignore then
					for ix = tokens[1].level + 2, #label_context do
						label_context[ix] = nil
					end
				else
					for ix = tokens[1].level + 1, #label_context do
						label_context[ix] = nil
					end
					labels[tokens[1].value] = tostring(output_pointer)
					label_context[tokens[1].level + 1] = tokens[1].rest
				end
				table.remove(tokens, 1)
				table.remove(tokens, 1)
			end

			if #tokens >= 1 and tokens[1]:is("mnemonic") then
				local funcs = tokens[1].mnemonic
				local params = parameters(tokens[1], tokens, 2, #tokens)
				local params_flc = {}
				for px = 1, #params do
					local param = params[px]
					local param_flc = {}
					for tx = 1, #param do
						table.insert(param_flc, param[tx]:clone())
					end
					label_offsets(true, param_flc)
					evaluations(true, param_flc)
					numbers(true, param_flc)
					table.insert(params_flc, param_flc)
				end
				local ok, length = funcs.length(tokens[1], params_flc)
				if ok then
					local overwrites = {}
					for ix = output_pointer, output_pointer + length - 1 do
						local overwritten = to_emit[ix]
						if overwritten then
							overwrites[overwritten.head] = true
						end
					end
					if next(overwrites) then
						local overwritten_count = 0
						for _ in pairs(overwrites) do
							overwritten_count = overwritten_count + 1
						end
						tokens[1]:blamef(printf.warn, "opcode emitted here (offs 0x%04X, size %i) overwrites the following %i opcodes:", output_pointer, length, overwritten_count)
						for overwritten in pairs(overwrites) do
							overwritten.emitted_by:blamef(printf.info, "opcode emitted here (offs 0x%04X, size %i)", overwritten.offset, overwritten.length)
						end
					end
					to_emit[output_pointer] = {
						emit = funcs.emit,
						parameters = params,
						length = length,
						emitted_by = tokens[1],
						offset = output_pointer
					}
					to_emit[output_pointer].head = to_emit[output_pointer]
					for ix = output_pointer + 1, output_pointer + length - 1 do
						to_emit[ix] = {
							head = to_emit[output_pointer]
						}
					end
					output_pointer = output_pointer + length
				else
					line_failed = true
				end

			elseif #tokens >= 1 and tokens[1]:is("hook") then
				local parameters = parameters(tokens[1], tokens, 2, #tokens)
				for ix, ix_param in ipairs(parameters) do
					local evals_ok, ix, jx, err = evaluations(false, ix_param, labels)
					if evals_ok then
						local numbers_ok, ix, err = numbers(false, ix_param)
						if not numbers_ok then
							ix_param[ix]:blamef(printf.err, "invalid number: %s", err)
							line_failed = true
						end
					else
						ix_param[ix].value[jx]:blamef(printf.err, "evaluation failed: %s", err)
						line_failed = true
					end
				end
				if not line_failed then
					line_failed = not tokens[1].hook(tokens[1], parameters)
				end

			elseif #tokens == 0 then
				-- * Nothing.

			else
				tokens[1]:blamef(printf.err, "expected label declaration, instruction or hook invocation")
				line_failed = true

			end
		end

		if line_failed then
			printf.err_called = true
		end
	end
	if printf.err_called then
		printf.failf("instruction resolution stage failed, bailing")
	end

	return to_emit, labels, model_restrictions
end

return {
	parameters = parameters,
	numbers = numbers,
	label_offsets = label_offsets,
	evaluations = evaluations,
	instructions = instructions,
}

end

modules["tptasm.tokenise"] = function()
local printf = require("tptasm.printf")

local token_i = {}
local token_mt = { __index = token_i }

function token_i:is(type, value)
	return self.type == type and (not value or self.value == value)
end

function token_i:punctuator(...)
	return self:is("punctuator", ...)
end

function token_i:identifier(...)
	return self:is("identifier", ...)
end

function token_i:stringlit(...)
	return self:is("stringlit", ...)
end

function token_i:charlit(...)
	return self:is("charlit", ...)
end

function token_i:number()
	return self:is("number")
end

local function parse_number_base(str, base)
	local out = 0
	for ix, ch in str:gmatch("()(.)") do
		local pos = base:find(ch)
		if not pos then
			return false, ("invalid digit at position %i"):format(ix)
		end
		out = out * #base + (pos - 1)
		if out >= 0x100000000 then
			return false, "unsigned 32-bit overflow"
		end
	end
	return true, out
end

function token_i:parse_number()
	local str = self.value
	if str:match("^0[Xx][0-9A-Fa-f]+$") then
		return parse_number_base(str:sub(3):lower(), "0123456789abcdef")
	elseif str:match("^[0-9A-Fa-f]+[Hh]$") then
		return parse_number_base(str:sub(1, -2), "0123456789abcdef")
	elseif str:match("^0[Bb][0-1]+$") then
		return parse_number_base(str:sub(3), "01")
	elseif str:match("^0[Oo][0-7]+$") then
		return parse_number_base(str:sub(3), "01234567")
	elseif str:match("^[0-9]+$") then
		return parse_number_base(str, "0123456789")
	end
	return false, "notation not recognised"
end

function token_i:point(other)
	other.sline = self.sline
	other.soffs = self.soffs
	other.expanded_from = self.expanded_from
	return setmetatable(other, token_mt)
end

function token_i:blamef_after(report, format, ...)
	self.sline:blamef_after(report, self, format, ...)
end

function token_i:blamef(report, format, ...)
	report("%s:%i:%i: " .. format, self.sline.path, self.sline.line, self.soffs, ...)
	self.sline:dump_itop()
	if self.expanded_from then
		self.expanded_from:blamef(printf.info, "  expanded from this")
	end
end

function token_i:clone()
	local clone = setmetatable({}, token_mt)
	for key, value in pairs(self) do
		clone[key] = value
	end
	return clone
end

function token_i:expand_by(other)
	local clone = self:clone()
	clone.expanded_from = other
	return clone
end

local transition = {}
local all_8bit = ""
for ix = 0, 255 do
	all_8bit = all_8bit .. string.char(ix)
end
local function transitions(transition_list)
	local tbl = {}
	local function add_transition(cond, action)
		if type(cond) == "string" then
			for ch in all_8bit:gmatch(cond) do
				tbl[ch:byte()] = action
			end
		else
			tbl[cond] = action
		end
	end
	for _, ix_trans in ipairs(transition_list) do
		add_transition(ix_trans[1], ix_trans[2])
	end
	return tbl
end

transition.push = transitions({
	{         "'", { consume =  true, state = "charlit"    }},
	{        "\"", { consume =  true, state = "stringlit"  }},
	{     "[;\n]", { consume = false, state = "done"       }},
	{     "[0-9]", { consume =  true, state = "number"     }},
	{ "[_A-Za-z]", { consume =  true, state = "identifier" }},
	{ "[%[%]%(%)%+%-%*/%%:%?&#<>=!^~%.{}\\|@$,`]", { consume = false, state = "punctuator" }},
})
transition.identifier = transitions({
	{ "[_A-Za-z0-9]", { consume =  true, state = "identifier" }},
	{          false, { consume = false, state = "push"       }},
})
transition.number = transitions({
	{ "[_A-Za-z0-9]", { consume =  true, state = "number" }},
	{          false, { consume = false, state = "push"   }},
})
transition.charlit = transitions({
	{  "'", { consume = true, state = "push"         }},
	{ "\n", { error = "unfinished character literal" }},
})
transition.stringlit = transitions({
	{ "\"", { consume = true, state = "push"      }},
	{ "\n", { error = "unfinished string literal" }},
})
transition.punctuator = transitions({
	{ ".", { consume = true, state = "push" }},
})

local whitespace = {
	[("\f"):byte()] = true,
	[("\n"):byte()] = true,
	[("\r"):byte()] = true,
	[("\t"):byte()] = true,
	[("\v"):byte()] = true,
	[(" " ):byte()] = true
}

return function(sline)
	local line = sline.str .. "\n"
	local tokens = {}
	local state = "push"
	local token_begin
	local cursor = 1
	while cursor <= #line do
		local ch = line:byte(cursor)
		if state == "push" and whitespace[ch] and #tokens > 0 then
			tokens[#tokens].whitespace_follows = true
		end
		local old_state = state
		local transition_info = transition[state][ch] or transition[state][false]
		local consume = true
		if transition_info then
			if transition_info.error then
				return false, cursor, transition_info.error
			end
			state = transition_info.state
			consume = transition_info.consume
		end
		if consume then
			cursor = cursor + 1
		end
		if state == "done" then
			break
		end
		if old_state == "push" and state ~= "push" then
			token_begin = cursor
			if consume then
				token_begin = token_begin - 1
			end
		end
		if old_state ~= "push" and state == "push" then
			local token_end = cursor - 1
			table.insert(tokens, setmetatable({
				type = old_state,
				value = line:sub(token_begin, token_end),
				sline = sline,
				soffs = token_begin
			}, token_mt))
		end
	end
	if #tokens > 0 then
		tokens[#tokens].whitespace_follows = true
	end
	return true, tokens
end

end

modules["tptasm.utility"] = function()
local printf = require("tptasm.printf")

local function get_line(up)
	local _, err = pcall(error, "@", up + 2)
	return err:match("^(.-)%s*:%s*@$")
end

local function parse_args(args)
	local named_args = {}
	local unnamed_args = {}
	if #args == 1 and type(args[1]) == "table" then
		for _, arg in ipairs(args[1]) do
			table.insert(unnamed_args, arg)
		end
		for key, arg in pairs(args[1]) do
			if type(key) ~= "number" then
				named_args[key] = arg
			end
		end
	else
		local max_arg = 0
		for key in pairs(args) do
			max_arg = key
		end
		local unnamed_counter = 0
		for ix_arg = 1, max_arg do
			local arg = args[ix_arg]
			local key_value = type(arg) == "string" and { arg:match("^([^=]+)=(.+)$") }
			if key_value and key_value[1] then
				if named_args[key_value[1]] then
					printf.warn("argument #%i overrides earlier specification of %s", ix_arg, key_value[1])
				end
				named_args[key_value[1]] = key_value[2]
			else
				unnamed_counter = unnamed_counter + 1
				unnamed_args[unnamed_counter] = arg
			end
		end
	end
	return named_args, unnamed_args
end

local function resolve_relative(base_with_file, relative)
	local components = {}
	local parent_depth = 0
	-- * TODO: support more prefixes (i.e. C:, eww)
	local prefix, concatenated_path = (base_with_file .. "/../" .. relative):match("^(/?)(.+)$")
	for component in concatenated_path:gmatch("[^/]+") do
		if component == ".." then
			if #components > 0 then
				components[#components] = nil
			else
				parent_depth = parent_depth + 1
			end
		elseif component ~= "." then
			table.insert(components, component)
		end
	end
	for _ = 1, parent_depth do
		table.insert(components, 1, "..")
	end
	return prefix .. table.concat(components, "/")
end

local function utf8_each(str)
	local cursor = 0
	return function()
		if cursor >= #str then
			return
		end
		cursor = cursor + 1
		local head = str:byte(cursor)
		if head < 0x80 then
			return cursor, cursor, head
		end
		if head < 0xE0 and cursor + 1 <= #str then
			local cont1 = str:byte(cursor + 1, cursor + 1)
			cursor = cursor + 1
			return cursor - 1, cursor, head % 0x20 * 0x40 + cont1 % 0x40
		end
		if head < 0xF0 and cursor + 2 <= #str then
			local cont1, cont2 = str:byte(cursor + 1, cursor + 2)
			cursor = cursor + 2
			return cursor - 2, cursor, head % 0x10 * 0x1000 + cont1 % 0x40 * 0x40 + cont2 % 0x40
		end
		if head < 0xF8 and cursor + 3 <= #str then
			local cont1, cont2, cont3 = str:byte(cursor + 1, cursor + 3)
			cursor = cursor + 3
			return cursor - 3, cursor, head % 0x08 * 0x40000 + cont1 % 0x40 * 0x1000 + cont2 % 0x40 * 0x40 + cont3 % 0x40
		end
	end
end

return {
	get_line = get_line,
	parse_args = parse_args,
	resolve_relative = resolve_relative,
	utf8_each = utf8_each,
}

end

modules["tptasm.xbit32"] = function()
local function sub(a, b)
	local s = a - b
	if s < 0 then
		s = s + 0x100000000
	end
	return s
end

local function add(a, b)
	local s = a + b
	if s >= 0x100000000 then
		s = s - 0x100000000
	end
	return s
end

local function divmod(a, b)
	local quo = math.floor(a / b)
	return quo, a - quo * b
end

local function div(a, b)
	local quo, rem = divmod(a, b)
	return quo
end

local function mod(a, b)
	local quo, rem = divmod(a, b)
	return rem
end

local function hasbit(a, b)
	return a % (b + b) >= b
end

local function band(a, b)
	local curr = 1
	local out = 0
	for ix = 0, 31 do
		if hasbit(a, curr) and hasbit(b, curr) then
			out = out + curr
		end
		curr = curr * 2
	end
	return out
end

local function mul(a, b)
	local ll = band(a, 0xFFFF) * band(b, 0xFFFF)
	local lh = band(band(a, 0xFFFF) * math.floor(b / 0x10000), 0xFFFF)
	local hl = band(math.floor(a / 0x10000) * band(b, 0xFFFF), 0xFFFF)
	return add(add(ll, lh * 0x10000), hl * 0x10000)
end

local function bor(a, b)
	local curr = 1
	local out = 0
	for ix = 0, 31 do
		if hasbit(a, curr) or hasbit(b, curr) then
			out = out + curr
		end
		curr = curr * 2
	end
	return out
end

local function bxor(a, b)
	local curr = 1
	local out = 0
	for ix = 0, 31 do
		if hasbit(a, curr) ~= hasbit(b, curr) then
			out = out + curr
		end
		curr = curr * 2
	end
	return out
end

local function lshift(a, b)
	if b >= 32 then
		return 0
	end
	return mul(a, 2 ^ b)
end

local function rshift(a, b)
	if b >= 32 then
		return 0
	end
	return div(a, 2 ^ b)
end

return {
	bxor = bxor,
	bor = bor,
	band = band,
	sub = sub,
	add = add,
	mul = mul,
	div = div,
	mod = mod,
	rshift = rshift,
	lshift = lshift,
}

end


return (function(...)
	local first_mod_name = "tptasm"	local senv
	local _ENV = _ENV
	if _G.rawget(_G, "setfenv") then
		senv = _G.getfenv(1)
		_G.setfenv(1, _G)
	else
		senv = _ENV
		_ENV = _G
	end
	local modules = senv.modules
	senv.modules = nil

	local unpack = rawget(_G, "unpack") or table.unpack
	local function packn(...)
		return { [ 0 ] = select("#", ...), ... }
	end
	local function unpackn(tbl, from, to)
		return unpack(tbl, from or 1, to or tbl[0])
	end
	local chunkname = debug.getinfo(1, "S").source
	if chunkname:find("^[=@]") then
		chunkname = chunkname:sub(2)
	else
		chunkname = nil
	end
	local lineinfo = {
		{ mod_name = "tptasm.xbit32", line = 4653, mod_lines = 104 },
		{ mod_name = "tptasm.utility", line = 4550, mod_lines = 99 },
		{ mod_name = "tptasm.tokenise", line = 4346, mod_lines = 200 },
		{ mod_name = "tptasm.resolve", line = 3779, mod_lines = 563 },
		{ mod_name = "tptasm.printf", line = 3697, mod_lines = 78 },
		{ mod_name = "tptasm.preprocess", line = 3162, mod_lines = 531 },
		{ mod_name = "tptasm.opcode", line = 3098, mod_lines = 60 },
		{ mod_name = "tptasm.evaluate", line = 2922, mod_lines = 172 },
		{ mod_name = "tptasm.emit", line = 2853, mod_lines = 65 },
		{ mod_name = "tptasm.detect", line = 2594, mod_lines = 255 },
		{ mod_name = "tptasm.config", line = 2560, mod_lines = 30 },
		{ mod_name = "tptasm.archs.r3", line = 2203, mod_lines = 353 },
		{ mod_name = "tptasm.archs.r2", line = 1793, mod_lines = 406 },
		{ mod_name = "tptasm.archs.ptp7", line = 1635, mod_lines = 154 },
		{ mod_name = "tptasm.archs.micro21", line = 1392, mod_lines = 239 },
		{ mod_name = "tptasm.archs.maps", line = 1183, mod_lines = 205 },
		{ mod_name = "tptasm.archs.i8m7d28s", line = 947, mod_lines = 232 },
		{ mod_name = "tptasm.archs.hacks", line = 867, mod_lines = 76 },
		{ mod_name = "tptasm.archs.b29k1qs60", line = 719, mod_lines = 144 },
		{ mod_name = "tptasm.archs.armatoste", line = 426, mod_lines = 289 },
		{ mod_name = "tptasm.archs.a728d28", line = 237, mod_lines = 185 },
		{ mod_name = "tptasm.archs", line = 191, mod_lines = 42 },
		{ mod_name = "tptasm", line = 8, mod_lines = 179 }
	}
	local function escape_regex(str)
		return (str:gsub("[%$%%%(%)%*%+%-%.%?%]%[%^]", "%%%1"))
	end
	local function demangle(str)
		if chunkname then
			return (str:gsub(escape_regex(chunkname) .. ":(%d+)", function(line)
				line = tonumber(line)
				for _, info in ipairs(lineinfo) do
					if info.line <= line then
						local mod_line = line - info.line + 1
						if mod_line <= info.mod_lines then
							return ("%s$%s:%i"):format(chunkname, info.mod_name, mod_line)
						end
					end
				end
			end))
		end
		return str
	end
	local function traceback(...)
		return demangle(debug.traceback(...))
	end
	local function xpcall_wrap(func, handler)
		return function(...)
			local iargs = packn(...)
			local oargs
			xpcall(function()
				oargs = packn(func(unpackn(iargs)))
			end, function(err)
				if type(err) == "string" then
					err = demangle(err)
				end
				if handler then
					handler(err)
				end
				if type(err) == "string" then
					print(traceback(err, 2))
				end
				return err
			end)
			if oargs then
				return unpackn(oargs)
			end
		end
	end

	local mod_state = {}
	local mod_result = {}
	local function modulepack_require(mod_name)
		if mod_state[mod_name] ~= "loaded" then
			local func = modules[mod_name]
			if not func then
				error(("module %q not found"):format(mod_name), 2)
			end
			if mod_state[mod_name] == "loading" then
				error("circular dependency", 2)
			end
			mod_state[mod_name] = "loading"
			local ok = true
			mod_result[mod_name] = xpcall_wrap(func, function()
				ok = false
			end)()
			if not ok then
				mod_state[mod_name] = "failed"
				error("module failed", 2)
			end
			mod_state[mod_name] = "loaded"
		end
		return mod_result[mod_name]
	end

	for key, value in pairs(_G) do
		rawset(senv, key, value)
	end
	-- Lua 5.4 removed global unpack; provide it for sandbox (e.g. tptasm.evaluate)
	if not rawget(senv, "unpack") then
		rawset(senv, "unpack", table.unpack)
	end
	rawset(senv, "require", modulepack_require)
	setmetatable(senv, { __index = function(_, key)
		error(("__index on env: %s"):format(tostring(key)), 2)
	end, __newindex = function(_, key)
		error(("__newindex on env: %s"):format(tostring(key)), 2)
	end })

	mod_result["modulepack"] = {
		packn       = packn,
		unpackn     = unpackn,
		xpcall_wrap = xpcall_wrap,
		demangle    = demangle,
		traceback   = traceback,
	}
	mod_state["modulepack"] = "loaded"

	local ok = true
	local ret = packn(xpcall_wrap(function(...)
		modulepack_require(first_mod_name).run(...)
	end, function()
		ok = false
	end)(...))
	if not ok then
		error("entry point failed", 2)
	end
	return unpackn(ret)
end)(...)
