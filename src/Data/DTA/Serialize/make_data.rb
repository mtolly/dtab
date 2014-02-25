class String
  def to_type
    self
  end
end

class Maybe
  def initialize(of_type)
    @of_type = of_type
  end

  def to_type
    "Maybe #{@of_type}"
  end
end

def maybe(t)
  Maybe.new(t)
end

# Makes a DTA record data-type with ToChunks and FromChunks instances.
# The fields is an array of arrays of the form:
#   ['dta_key', 'HsConstructor' (or nil), 'HsType', 'haddock comment' (optional)]
def makeRecord(name, fields)
  fields = fields.map do |f|
    if f[1].nil? or f[1].empty?
      f = f.dup
      f[1] = f[0].gsub(/_([a-z])/) { $1.upcase }
      f
    else
      f
    end
  end

  records = fields.map do |f|
    str = "#{f[1]} :: #{f[2].to_type}"
    str += " {- ^ #{f[3]} -}" if f[3]
    str
  end
  deriving = 'deriving (Eq, Ord, Read, Show)'
  data = "data #{name} = #{name} { #{records.join(', ')} } #{deriving}"

  to_records = fields.map do |f|
    case f[2]
    when String
      "[(#{f[0].inspect}, toChunks $ #{f[1]} x)]"
    when Maybe
      "(case #{f[1]} x of { Nothing -> []; Just v -> [(#{f[0].inspect}, toChunks v)] })"
    else
      STDERR.puts "Invalid type descriptor: #{f[2].inspect}"
      exit 1
    end
  end
  to_defn =
    "toChunks x = makeDict $ Dict $ Map.fromList $ #{to_records.join(' ++ ')}"
  to_instance = "instance ToChunks #{name} where { #{to_defn} }"

  from_records = fields.map do |f|
    case f[2]
    when String
      "(dictLookup #{f[0].inspect} d >>= fromChunks)"
    when Maybe
      "(case dictLookup #{f[0].inspect} d of { Left _ -> Right Nothing; Right v -> fmap Just $ fromChunks v })"
    else
      STDERR.puts "Invalid type descriptor: #{f[2].inspect}"
      exit 1
    end
  end
  from_defn =
    "fromChunks = getDict >=> \\d -> #{name} <$> #{from_records.join(' <*> ')}"
  from_instance = "instance FromChunks #{name} where { #{from_defn} }"

  [data, to_instance, from_instance].join("\n\n")
end

# Makes a DTA enumeration data-type with ToChunks and FromChunks instances.
# The fields is an array of arrays of the form:
#   ['dta_key', 'HsConstructor' (nil or optional), 'haddock comment' (optional)]
def makeEnum(name, fields, cons = 'Key')
  fields = fields.map do |f|
    if f[1].nil? or f[1].empty?
      f = f.dup
      f[1] = f[0].gsub(/(^|_)([a-z])/) { $2.upcase }
      f
    else
      f
    end
  end

  records = fields.map do |f|
    str = f[1]
    str += " {- ^ #{f[2]} -}" if f[2]
    str
  end
  deriving = 'deriving (Eq, Ord, Read, Show, Enum, Bounded)'
  data = "data #{name} = #{records.join(' | ')} #{deriving}"

  to_lines = fields.map { |f| "toChunks #{f[1]} = [#{cons} #{f[0].inspect}]" }
  to_instance = "instance ToChunks #{name} where { #{to_lines.join('; ')} }"

  from_lines = fields.map { |f| "fromChunks [#{cons} #{f[0].inspect}] = Right #{f[1]}" }
  from_lines << "fromChunks cs = Left $ \"Couldn't read as #{name}: \" ++ show cs"
  from_instance = "instance FromChunks #{name} where { #{from_lines.join('; ')} }"

  [data, to_instance, from_instance].join("\n\n")
end
