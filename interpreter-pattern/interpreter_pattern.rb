# What is the interpreter design pattern?
#
# - It is used to convert one representation of data into another
# - The Context contains the information that will be interpreted
# - The Expression is an abstract class that defines all the methods
#   needed to perform the different conversions
# - The Terminal or Concrete Expressions provide specific conversions on
#   different types of data
#
# @see http://www.newthinktank.com/2012/10/interpreter-design-pattern-tutorial/

class Conversion
  attr_reader :from_unit, :to_unit, :amount

  def initialize(question)
    words = question.split(' ')
    @amount = words[0].to_i
    @from_unit = normalized_unit(words[1])
    @to_unit = normalized_unit(words[3])
  end

  def compute_response
    expr = ExpressionFactory.new(from_unit).make_expression
    resulting_amount = expr.send(to_unit, amount)

    "#{amount} #{from_unit} equals #{resulting_amount} #{to_unit}"
  end

  private

  def normalized_unit(unit)
    unit = unit.downcase
    if unit.end_with? 's'
      unit
    else
      unit + 's'
    end
  end
end

class Bit
  def bits(amount)
    amount
  end

  def nibbles(amount)
    amount / 4
  end

  def bytes(amount)
    amount / 8
  end
end

class Nibble
  def bits(amount)
    amount * 4
  end

  def nibbles(amount)
    amount
  end

  def bytes(amount)
    amount / 2
  end
end

class Byte
  def bits(amount)
    amount * 8
  end

  def nibbles(amount)
    amount * 2
  end

  def bytes(amount)
    amount
  end
end

class ExpressionFactory
  def initialize(unit)
    @unit = unit
  end

  def make_expression
    case @unit
    when 'bits'
      Bit.new
    when 'nibbles'
      Nibble.new
    when 'bytes'
      Byte.new
    end
  end
end

################################################################################

puts Conversion.new('8 bits to bytes').compute_response
