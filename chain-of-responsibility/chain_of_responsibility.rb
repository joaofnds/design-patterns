# What is the chain of responsibility design pattern?
#
# - This pattern send data to an object and if that object can't use it,
# it sends it to any number of other objects that may be able to use it
#    - Create 4 objects that can either add, subtract, multiply, or divide
#    - Send 2 numbers and a command and allow these 4 object to decide
#      which can handle the requested calculation
#
# @see http://www.newthinktank.com/2012/10/chain-of-responsibility-design-pattern-tutorial/

class Chain
  def set_next_chain(chain)
    raise NotImplementedError
  end

  def calculate(request)
    raise NotImplementedError
  end
end

class Numbers
  attr_reader :a, :b, :op

  def initialize(a, b, op)
    @a = a
    @b = b
    @op = op
  end
end

class AddNumbers < Chain
  attr_accessor :next_in_chain

  def calculate(request)
    if request.op == :add
      puts "#{request.a} + #{request.b} = #{request.a + request.b}"
    else
      next_in_chain.calculate(request)
    end
  end
end

class SubtractNumbers < Chain
  attr_accessor :next_in_chain

  def calculate(request)
    if request.op == :subtract
      puts "#{request.a} - #{request.b} = #{request.a - request.b}"
    else
      next_in_chain.calculate(request)
    end
  end
end

class MultiplyNumbers < Chain
  attr_accessor :next_in_chain

  def calculate(request)
    if request.op == :multiply
      puts "#{request.a} * #{request.b} = #{request.a * request.b}"
    else
      next_in_chain.calculate(request)
    end
  end
end

class DivideNumbers < Chain
  attr_accessor :next_in_chain

  def calculate(request)
    if request.op == :divide
      puts "#{request.a} / #{request.b} = #{request.a / request.b.to_f}"
    else
      next_in_chain.calculate(request)
    end
  end
end

class EndOfChain < Chain
  def calculate(_request)
    puts 'reached the end of chain'
  end
end

################################################################################

chain_calc_1 = AddNumbers.new
chain_calc_2 = SubtractNumbers.new
chain_calc_3 = MultiplyNumbers.new
chain_calc_4 = DivideNumbers.new
chain_calc_5 = EndOfChain.new

chain_calc_1.next_in_chain = chain_calc_2
chain_calc_2.next_in_chain = chain_calc_3
chain_calc_3.next_in_chain = chain_calc_4
chain_calc_4.next_in_chain = chain_calc_5

request = Numbers.new(1, 2, :add)
chain_calc_1.calculate(request)

chain_calc_1.calculate(Numbers.new(1, 2, :subtract))
chain_calc_1.calculate(Numbers.new(1, 2, :multiply))
chain_calc_1.calculate(Numbers.new(1, 2, :divide))
