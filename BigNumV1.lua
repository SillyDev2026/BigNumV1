local Bn = {}
type Bn = {man: number, exp: number}
Bn.Zero = {man = 0, exp = 0}
Bn.Inf = {man = 1, exp = math.huge}
Bn.NaN = {man = 0, exp = -math.huge}
Bn.One = {man=1, exp = 0}
Bn.NegInf = {man=-1, exp = math.huge}

function Bn.toStr(val): string
	if val == Bn.Inf then
		return 'Inf'
	elseif val == Bn.NaN then
		return 'NaN'
	elseif val == Bn.NegInf then
		return '-Inf'
	elseif val == Bn.Zero then
		return '0'
	end
	return val.man .. 'e' .. val.exp
end

function Bn.new(man: number, exp: number): Bn
	if man == 0 then
		return Bn.Zero
	elseif man ~= man or exp ~= exp then
		return Bn.NaN
	elseif man >= 100 and exp == math.huge then
		return Bn.Inf
	elseif exp <= -1e308 then
		return Bn.Zero
	elseif exp >= 1e308 then
		return Bn.Inf
	end
	if exp >= 1e308 then return Bn.Inf elseif exp <= -1e308 then return Bn.Zero end
	return {man = man, exp = exp}
end

function Bn.toNumber(val: Bn): number
	if val == Bn.Zero then
		return 0
	elseif  val == Bn.Inf then
		return math.huge
	elseif val == Bn.NaN then
		return 0/0
	end
	local man, exp = val.man, val.exp
	local sign = val.man < 0 and -1 or 1
	local maxE = 308
	local minE = -308
	if exp > maxE then
		return sign*math.huge
	elseif exp < minE then
		return 0
	end
	local intE = math.floor(exp)
	local fracE = exp - intE
	local res = 10^fracE
	local chunk = 308
	while intE ~= 0 do
		local step = math.min(chunk, intE)
		res*=10^step
		intE-=step
	end
	return sign * res
end

function Bn.fromNumber(val: number): Bn
	if val == 0 then
		return Bn.Zero
	elseif val == math.huge then
		return Bn.Inf
	elseif val ~= val then
		return Bn.NaN
	end
	local sign = val < 0 and -1 or 1
	val = math.abs(val)
	local exp = math.log10(val)
	local man = 1
	local maxE = 1e308
	local minE = -1e308
	if exp > maxE then
		return sign > 0 and Bn.Inf or Bn.NegInf
	elseif exp < minE then
		return Bn.Zero
	end
	return {man=sign*man, exp=exp}
end

function Bn.fromString(val: string): Bn
	local trim = val:match("^%s*(.-)%s*$")
	local lower = trim:lower()
	if lower == "0" then return Bn.Zero end
	if lower == "inf" then return Bn.Inf end
	if lower == "-inf" then return Bn.NegInf end
	if lower == "nan" then return Bn.NaN end
	local sign = 1
	if trim:sub(1,1) == "-" then
		sign = -1
		trim = trim:sub(2)
	elseif trim:sub(1,1) == "+" then
		trim = trim:sub(2)
	end
	local parts = {}
	for part in trim:gmatch('[^eE]+') do
		table.insert(parts, part)
	end
	local base = tonumber(parts[1])
	if not base then return Bn.NaN end
	local exp = 0
	if #parts == 2 then
		exp = tonumber(parts[2]) or 0
	elseif #parts >= 3 then
		exp = tonumber(parts[2]) or 0
		for i = 2, #parts do
			local nextExp = tonumber(parts[i]) or 0
			exp = i == 2 and nextExp or 10^exp + nextExp
		end
	end
	local totalExp = math.log10(math.abs(base)) + exp
	return {man = sign, exp = totalExp}
end

function Bn.convert(val: any): Bn
	if typeof(val) == 'table' then
		if val.man ~= nil and val.exp ~= nil then
			return val
		elseif val[1] ~= nil and val[2] ~= nil then
			return Bn.new(val[1], val[2])
		else
			return Bn.Zero
		end
	elseif typeof(val) == 'number' then
		return Bn.fromNumber(val)
	elseif typeof(val) == 'string' then
		return Bn.fromString(val)
	end
	warn('Failed to convert to Bn autoconverted to {0, 0}')
	return Bn.Zero
end

function Bn.add(val1: any, val2: any): Bn
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1 == Bn.NaN or val2 == Bn.NaN then return Bn.NaN end
	if val1 == Bn.Inf or val2 == Bn.Inf then
		if (val1.man == 1 and val1.exp == math.huge and val2.man == -1 and val2.exp == math.huge)
			or (val2.man == 1 and val2.exp == math.huge and val1.man == -1 and val1.exp == math.huge) then
			return Bn.NaN
		end
		return Bn.Inf
	end
	if val1 == Bn.Zero then return val2 elseif val2 == Bn.Zero then return val1 end
	local e1, e2 = val1.exp, val2.exp
	local m1, m2 = val1.man, val2.man
	if e1 > e2 then
		local diff = 10^(e2-e1)
		local res = m1+m2*diff
		return Bn.new(1, math.log10(res)+e1)
	end
	local diff = 10^(e1-e2)
	local res = m2+m1*diff
	return Bn.new(1, math.log10(res)+e2)
end

function Bn.neg(val: Bn): Bn
	val = Bn.convert(val)
	return {man=-val.man, exp=val.exp}
end

function Bn.sub(val1: any, val2: any): Bn
	return Bn.add(val1, Bn.neg(val2))
end

function Bn.recip(val: any): Bn
	val = Bn.convert(val)
	return Bn.new(1, -val.exp)
end

function Bn.mul(val1: any, val2: any): Bn
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1 == Bn.NaN or val2 == Bn.NaN then
		return Bn.NaN
	elseif val1 == Bn.Zero and val2 == Bn.Zero then
		return Bn.NaN
	elseif val2 == Bn.Zero then
		return val1.man < 0 and Bn.NegInf or Bn.Inf
	elseif val1 == Bn.Zero then
		return Bn.Zero
	elseif val1 == Bn.Inf and val2 == Bn.Inf then
		return Bn.NaN
	elseif val1 == Bn.Inf then
		return Bn.Inf
	elseif val2 == Bn.Inf then
		return Bn.Zero
	end
	local exp = val1.exp + val2.exp
	return Bn.new(1, exp)
end

function Bn.div(val1: any, val2: any)
	return Bn.mul(val1, Bn.recip(val2))
end

function Bn.pow(val1, val2)
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1 == Bn.NaN or val2 == Bn.NaN then
		return Bn.NaN
	end
	if val1 == Bn.Zero then
		if val2 == Bn.Zero then
			return Bn.NaN
		elseif val2.man > 0 then
			return Bn.Zero
		else
			return Bn.Inf
		end
	end
	if val1 == Bn.Inf then
		if val2.man > 0 then
			return Bn.Inf
		elseif val2.man < 0 then
			return Bn.Zero
		else
			return Bn.NaN
		end
	end
	if val2 == Bn.Zero then
		return Bn.new(1,0)
	elseif val2 == Bn.Inf then
		if val1.man > 1 or val1.exp > 0 then
			return Bn.Inf
		elseif val1.man == 1 and val1.exp == 0 then
			return Bn.NaN
		else
			return Bn.Zero
		end
	end
	if val1.man < 0 and (val2.exp ~= 0 or val2.man % 1 ~= 0) then
		return Bn.NaN
	end
	local newExp = val1.exp * (val2.man * 10^val2.exp)
	return Bn.new(1, newExp)
end

function Bn.logn(val: any): Bn
	val = Bn.convert(val)
	if val == Bn.NaN then
		return Bn.NaN
	elseif val == Bn.Zero then
		return Bn.NegInf
	elseif val.man < 0 then
		return Bn.NaN
	elseif val == Bn.Inf then
		return Bn.Inf
	end
	local log = math.log10(math.abs(val.exp)/0.4342944819032518)
	return Bn.new(math.sign(val.exp), log)
end

function Bn.log(val1: any, val2: any): Bn
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1 == Bn.NaN or val2 == Bn.NaN then
		return Bn.NaN
	elseif val1 == Bn.Zero then
		return Bn.NegInf
	elseif val1.man < 0 then
		return Bn.NaN
	elseif val2.man <= 0 then
		return Bn.NaN
	elseif val1 == Bn.Inf and val2 == Bn.Inf then
		return Bn.NaN
	elseif val1 == Bn.Inf then
		return Bn.Inf
	elseif val2 == Bn.Inf then
		return Bn.Zero
	end
	local l1 = math.log10(val1.man) + val1.exp
	local l2 = math.log10(val2.man) + val2.exp
	local res = l1/l2
	return Bn.new(1, res)
end

function Bn.log10(val: any): Bn
	val = Bn.convert(val)
	if val == Bn.NaN then
		return Bn.NaN
	elseif val == Bn.Zero then
		return Bn.NegInf
	elseif val.man < 0 then
		return Bn.NaN
	elseif val == Bn.Inf then
		return Bn.Inf
	end
	local l10 = math.log10(val.man) + val.exp
	return Bn.new(1, l10)
end

function Bn.pow10(val: any): Bn
	val = Bn.convert(val)
	if val == Bn.NaN then
		return Bn.NaN
	elseif val == Bn.Zero then
		return Bn.One
	elseif val == Bn.Inf then
		return Bn.Inf
	end
	local exp = val.man*10^val.exp
	return Bn.new(1, exp)
end

function Bn.cmp(a: any, b: any): number
	a, b = Bn.convert(a), Bn.convert(b)
	if a == Bn.Inf then return b == Bn.Inf and 0 or 1 end
	if b == Bn.Inf then return -1 end
	if a == Bn.NegInf then return b == Bn.NegInf and 0 or -1 end
	if b == Bn.NegInf then return 1 end
	if a == Bn.Zero and b == Bn.Zero then return 0 end
	local signA, signB = a.man < 0 and -1 or 1, b.man < 0 and -1 or 1
	if signA ~= signB then
		return signA > signB and 1 or -1
	end
	if a.exp ~= b.exp then
		return (a.exp > b.exp and 1 or -1) * signA
	end
	if a.man ~= b.man then
		return (a.man > b.man and 1 or -1) * signA
	end
	return 0
end

function Bn.eq(val1: any, val2: any): boolean
	return Bn.cmp(val1, val2) == 0
end

function Bn.le(val1: any, val2: any): boolean
	return Bn.cmp(val1, val2) == -1
end

function Bn.leeq(val1: any, val2: any): boolean
	return Bn.cmp(val1, val2) ~= 1
end

function Bn.me(val1: any, val2: any): boolean
	return Bn.cmp(val1, val2) == 1
end

function Bn.meeq(val1: any, val2: any): boolean
	return Bn.cmp(val1, val2) ~= -1
end

function Bn.showDigits(val, digits: number?): number
	digits = digits or 2
	return math.floor(val*10^digits:: number) / 10^digits:: number
end

function Bn.AddComma(val): string
	val = Bn.toNumber(Bn.convert(val))
	local left, num, right = tostring(val):match('^([^%d]*%d)(%d*)(.-)$')
	num = num:reverse():gsub('(%d%d%d)', '%1,')
	return left .. num:reverse() .. right
end

function Bn.short(val, digits, canComma)
	canComma = canComma or false
	val = Bn.convert(val)
	if val == Bn.NaN then return "NaN" end
	if val == Bn.Inf then return "inf" end
	if val == Bn.Zero then return "0" end
	local man, exp = val.man, val.exp
	local sign = man < 0 and "-" or ""
	man = math.abs(man)
	local SNumber = exp
	local leftover = SNumber % 3
	local baseVal = man * 10^leftover
	SNumber = math.floor(SNumber / 3)

	local base = {"", "k", "m", "b"}
	if SNumber <= #base-1 then
		local numStr = Bn.showDigits(baseVal, digits)
		if canComma and SNumber == 0 then
			return Bn.AddComma(numStr)
		else
			return sign .. numStr .. base[SNumber+1]
		end
	end
	local txt = ""
	local FirBigNumOnes = {"", "U","D","T","Qd","Qn","Sx","Sp","Oc","No"}
	local SecondOnes = {"", "De","Vt","Tg","qg","Qg","sg","Sg","Og","Ng"}
	local ThirdOnes = {"", "Ce", "Du","Tr","Qa","Qi","Se","Si","Ot","Ni"}
	local function suffixpart(n)
		local Hundreds = math.floor(n/100)
		n = n % 100
		local Tens = math.floor(n/10)
		local Ones = n % 10
		txt = txt .. FirBigNumOnes[Ones+1] .. SecondOnes[Tens+1] .. ThirdOnes[Hundreds+1]
	end
	if SNumber < 1000 then
		suffixpart(SNumber)
		return sign .. Bn.showDigits(baseVal, digits) .. txt
	end
	local MultOnes = {"Mi","Mc","Na","Pi","Fm","At","Zp","Yc", "Xo", "Ve", "Me", "Due", "Tre", "Te", "Pt", "He", "Hp", "Oct", "En", "Ic", "Mei", "Dui", "Tri", "Teti", "Pti", "Hei", "Hp", "Oci", "Eni", "Tra","TeC","MTc","DTc","TrTc","TeTc","PeTc","HTc","HpT","OcT","EnT","TetC","MTetc","DTetc","TrTetc","TeTetc","PeTetc","HTetc","HpTetc","OcTetc","EnTetc","PcT","MPcT","DPcT","TPCt","TePCt","PePCt","HePCt","HpPct","OcPct","EnPct","HCt","MHcT","DHcT","THCt","TeHCt","PeHCt","HeHCt","HpHct","OcHct","EnHct","HpCt","MHpcT","DHpcT","THpCt","TeHpCt","PeHpCt","HeHpCt","HpHpct","OcHpct","EnHpct","OCt","MOcT","DOcT","TOCt","TeOCt","PeOCt","HeOCt","HpOct","OcOct","EnOct","Ent","MEnT","DEnT","TEnt","TeEnt","PeEnt","HeEnt","HpEnt","OcEnt","EnEnt","Hect", "MeHect"}
	for i=#MultOnes,1,-1 do
		if SNumber >= 10^(i*3) then
			suffixpart(math.floor(SNumber / 10^(i*3))-1)
			txt = txt .. MultOnes[i+1]
			SNumber = SNumber % 10^(i*3)
		end
	end
	return sign .. Bn.showDigits(baseVal, digits) .. txt
end

return Bn