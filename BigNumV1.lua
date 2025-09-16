local Bn = {}
type Bn = {man: number, exp: number}
Bn.Zero = {man = 0, exp = 0}
Bn.Inf = {man = 1, exp = math.huge}
Bn.NaN = {man = 0, exp = -math.huge}
Bn.One = {man=1, exp = 0}
Bn.NegInf = {man=-1, exp = math.huge}

local alpha = {} do
	for i = string.byte('a'), string.byte('z') do
		table.insert(alpha, string.char(i))
	end
end

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
	local frac = exp - math.floor(exp)
	if frac ~= 0 then
		man *= 10^frac
		exp -= frac
	end
	local lm = math.log10(math.abs(man))
	local shift = math.floor(lm+1e-12)
	man/=10^shift
	exp+=shift
	if math.abs(man) >= 10 then
		man/=10
		exp+=1
	elseif math.abs(man) < 1 then
		man*=10
		exp-=1
	end
	return {man=man,exp=exp}
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
	local sign = man < 0 and -1 or 1
	man = math.abs(man)
	local maxE = 308
	local minE = -308
	if exp > maxE then
		return sign * math.huge
	elseif exp < minE then
		return 0
	end
	local intE = math.floor(exp)
	local fracE = exp-intE
	local safe = man*10^fracE
	local res = safe
	local chunk = 308
	while intE ~= 0 do
		local step = math.min(chunk, intE)
		res *= 10^step
		intE -= step
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
	local man, exp = math.abs(val), 0
	local maxE = 308
	local minE = -maxE
	if man >= 10^maxE then
		return sign > 0 and Bn.Inf or Bn.NegInf
	elseif man ~= 0 and man < 10^minE then
		return Bn.Zero
	end
	local lm = math.log10(man)
	local shift = math.floor(lm+1e-12)
	man/=10^shift
	exp=shift
	if man >= 10 then
		man/=10
		exp+=1
	elseif man < 1 then
		man*=10
		exp-=1
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
	local manStr, expStr = trim:match("([%d%.]+)[eE]?([+-]?%d*)")
	if not manStr then return Bn.NaN end
	expStr = expStr ~= "" and expStr or "0"
	local expVal = tonumber(expStr) or 0
	local pointPos = manStr:find("%.")
	local intPart, fracPart
	if pointPos then
		intPart = manStr:sub(1, pointPos-1)
		fracPart = manStr:sub(pointPos+1)
	else
		intPart = manStr
		fracPart = ""
	end
	intPart = intPart:gsub("^0+", "")
	fracPart = fracPart:gsub("0+$","")

	local manNumber
	if intPart ~= "" then
		expVal = expVal + #intPart - 1
		manNumber = intPart .. fracPart
	else
		local leadingZeros = fracPart:match("^0*") or ""
		manNumber = fracPart:gsub("^0+", "")
		expVal = expVal - #leadingZeros - 1
	end
	if manNumber == "" then return Bn.Zero end
	local m = tonumber(manNumber:sub(1,16)) or 1
	local ln = math.log10(m)
	local shift = math.floor(ln + 1e-12)
	m = m / 10^shift
	expVal = expVal + shift	
	if m >= 10 then
		m = m / 10
		expVal = expVal + 1
	elseif m < 1 then
		m = m * 10
		expVal = expVal - 1
	end
	return Bn.new(sign * m, expVal)
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
	if val1.exp > val2.exp then
		local diff = val1.exp-val2.exp
		local man = val2.man*10^(-diff)
		local result = val1.man+man
		return Bn.new(result, val1.exp)
	end
	local diff = val2.exp-val1.exp
	local man = val1.man*10^(-diff)
	local result = man+val2.man
	return Bn.new(result, val2.exp)
end

function Bn.neg(val: Bn): Bn
	val = Bn.convert(val)
	return {man=-val.man, exp=val.exp}
end

function Bn.sub(val1: any, val2: any): Bn
	return Bn.add(val1, Bn.neg(val2))
end

function Bn.div(val1: any, val2: any): Bn
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1 == Bn.NaN or val2 == Bn.NaN then
		return Bn.NaN
	elseif val1 == Bn.Zero and val2 == Bn.Zero then
		return Bn.NaN
	elseif val2 == Bn.Zero then
		return val1.man < 0 and {man=-1, exp=math.huge} or Bn.Inf
	elseif val1 == Bn.Zero then
		return Bn.Zero
	elseif val1 == Bn.Inf and val2 == Bn.Inf then
		return Bn.NaN
	elseif val1 == Bn.Inf then
		return Bn.Inf
	elseif val2 == Bn.Inf then
		return Bn.Zero
	end
	local man = val1.man / val2.man
	local exp = val1.exp - val2.exp
	return Bn.new(man, exp)
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
	local man = val1.man * val2.man
	local exp = val1.exp + val2.exp
	return Bn.new(man, exp)
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
	local logA = math.log10(math.abs(val1.man)) + val1.exp
	local exponent = val2.man * 10^val2.exp
	local r = logA * exponent
	local newMan = 10^(r % 1)
	local newExp = math.floor(r)
	if val1.man < 0 and val2.man % 2 == 1 and val2.exp == 0 then
		newMan = -newMan
	end
	return Bn.new(newMan, newExp)
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
	local log = math.log10(val.man) + val.exp
	return Bn.new(log%1, math.floor(log))
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
	return Bn.fromNumber(res)
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
	local l10 = val.exp + math.log10(val.man)
	return Bn.fromNumber(l10)
end

function Bn.pow10(val: any): Bn
	val = Bn.convert(val)
	if val == Bn.NaN then
		return Bn.NaN
	elseif val == Bn.Zero then
		return Bn.One
	elseif val == Bn.Inf then
		return Bn.Inf
	elseif val.man < 0 then
		local exp = -(val.man*10^val.exp)
		return Bn.new(1, exp)
	end
	local exp = val.man*10^val.exp
	return {man = 1, exp = exp}
end

function Bn.cmp(a: any, b: any): number
	a, b = Bn.convert(a), Bn.convert(b)
	if a == Bn.Inf then
		if b == Bn.Inf then return 0 else return - 1 end
	elseif b == Bn.Inf then
		return -1
	end
	if a == Bn.NegInf then
		if b == Bn.NegInf then return 0 else return -1 end
	elseif b == Bn.NegInf then
		return 1
	end
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

function Bn.short(val, digits, canComma: boolean?): string
	canComma = canComma or false
	val = Bn.convert(val)
	if val.man == -2 then return "NaN" end 
	if val.exp == 1e309 then return "inf" end
	local SNumber1: number, SNumber: number = val.man, val.exp
	local leftover = math.fmod(SNumber, 3)
	SNumber = math.floor(SNumber / 3)-1
	if SNumber <= -1 then return tostring(Bn.showDigits(SNumber1 * (10^leftover), digits)) end
	local base = {'k', 'm', 'b'}
	local FirBigNumOnes: {string} = {"", "U","D","T","Qd","Qn","Sx","Sp","Oc","No"}
	local SecondOnes: {string} = {"", "De","Vt","Tg","qg","Qg","sg","Sg","Og","Ng"}
	local ThirdOnes: {string} = {"", "Ce", "Du","Tr","Qa","Qi","Se","Si","Ot","Ni"}
	local MultOnes: {string} = {"", "Mi","Mc","Na","Pi","Fm","At","Zp","Yc", "Xo", "Ve", "Me", "Due", "Tre", "Te", "Pt", "He", "Hp", "Oct", "En", "Ic", "Mei", "Dui", "Tri", "Teti", "Pti", "Hei", "Hp", "Oci", "Eni", "Tra","TeC","MTc","DTc","TrTc","TeTc","PeTc","HTc","HpT","OcT","EnT","TetC","MTetc","DTetc","TrTetc","TeTetc","PeTetc","HTetc","HpTetc","OcTetc","EnTetc","PcT","MPcT","DPcT","TPCt","TePCt","PePCt","HePCt","HpPct","OcPct","EnPct","HCt","MHcT","DHcT","THCt","TeHCt","PeHCt","HeHCt","HpHct","OcHct","EnHct","HpCt","MHpcT","DHpcT","THpCt","TeHpCt","PeHpCt","HeHpCt","HpHpct","OcHpct","EnHpct","OCt","MOcT","DOcT","TOCt","TeOCt","PeOCt","HeOCt","HpOct","OcOct","EnOct","Ent","MEnT","DEnT","TEnt","TeEnt","PeEnt","HeEnt","HpEnt","OcEnt","EnEnt","Hect", "MeHect"}
	if canComma then
		if SNumber == 0 or SNumber == 1 then
			return Bn.AddComma(val)
		elseif SNumber == 2 then
			return tostring(Bn.showDigits(SNumber1 * (10^leftover), digits)) .. "b"
		end
	else
		local start = #base-1
		if SNumber <= start then
			return tostring(Bn.showDigits(SNumber1*(10^leftover), digits) .. base[SNumber+1])
		end
	end
	local txt: string = ""
	local function suffixpart(n: number)
		local Hundreds: number = math.floor(n/100)
		n = math.fmod(n, 100)
		local Tens: number = math.floor(n/10)
		n = math.fmod(n, 10)
		local Ones: number = math.floor(n/1)
		txt = txt .. FirBigNumOnes[Ones + 1]
		txt = txt .. SecondOnes[Tens + 1]
		txt = txt .. ThirdOnes[Hundreds + 1]
	end
	local function suffixpart2(n: number)
		if n > 0 then
			n = n + 1
		end
		if n > 1000 then
			n = math.fmod(n, 1000)
		end
		local Hundreds = math.floor(n/100)
		n = math.fmod(n, 100)
		local Tens = math.floor(n/10)
		n = math.fmod(n, 10)
		local Ones = math.floor(n/1)
		txt = txt .. FirBigNumOnes[Ones + 1]
		txt = txt .. SecondOnes[Tens + 1]
		txt = txt .. ThirdOnes[Hundreds + 1]
	end
	if SNumber < 1000 then
		suffixpart(SNumber)
		return tostring(Bn.showDigits(SNumber1 * (10^leftover), digits)) .. txt
	end
	for i=#MultOnes,0,-1 do
		if SNumber >= 10^(i*3) then
			suffixpart2(math.floor(SNumber / 10^(i*3))- 1)
			txt = txt .. MultOnes[i+1]
			SNumber = math.fmod(SNumber, 10^(i*3))
		end
	end
	return tostring(Bn.showDigits(SNumber1 * (10^leftover), digits)) .. txt
end

return Bn