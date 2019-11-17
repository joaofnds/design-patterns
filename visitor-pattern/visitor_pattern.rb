# What is the visitor design pattern?
#
# - Allows you to add methods to classes of different types without much
#   altering to those classes
# - You can make completely different methods depending on the class used
# - Allows you to define external classes that extend other classes without
#   majorly editing them
#
# @see http://www.newthinktank.com/2012/11/visitor-design-pattern-tutorial/

module Visitor
  def visit(visitable)
    raise NotImplementedError
  end
end

module Visitable
  def accept(visitor)
    raise NotImplementedError
  end
end

class Item
  include Visitable

  attr_reader :price

  def initialize(price)
    @price = price
  end

  def accept(visitor)
    visitor.visit(self)
  end
end

class Liquor < Item; end
class Tobacco < Item; end
class Necessity < Item; end

# Normal Tax
# ┌───────────┬────────┐
# │  (index)  │ Values │
# ├───────────┼────────┤
# │  liquor   │    18% │
# │  tobacco  │    32% │
# │ necessity │     0% │
# └───────────┴────────┘
class TaxVisitor
  include Visitor

  def visit(item)
    case item
    when Liquor
      '%.2f' % (item.price * 1.18)
    when Tobacco
      '%.2f' % (item.price * 1.32)
    when Necessity
      '%.2f' % item.price
    end
  end
end

# Holyday Tax
# ┌───────────┬────────┐
# │  (index)  │ Values │
# ├───────────┼────────┤
# │  liquor   │    10% │
# │  tobacco  │    30% │
# │ necessity │     0% │
# └───────────┴────────┘
class HolydayTaxVisitor
  include Visitor

  def visit(item)
    case item
    when Liquor
      '%.2f' % (item.price * 1.10)
    when Tobacco
      '%.2f' % (item.price * 1.30)
    when Necessity
      '%.2f' % item.price
    end
  end
end

################################################################################

tax = TaxVisitor.new
holyday_tax = HolydayTaxVisitor.new

milk = Necessity.new(3.47)
vodka = Liquor.new(11.99)
cigars = Tobacco.new(19.99)

puts 'milk --- $ ' + milk.accept(tax)
puts 'vodka -- $ ' + vodka.accept(tax)
puts 'cigars - $ ' + cigars.accept(tax)

puts "\nHolyday Prices"
puts 'milk --- $ ' + milk.accept(holyday_tax)
puts 'vodka -- $ ' + vodka.accept(holyday_tax)
puts 'cigars - $ ' + cigars.accept(holyday_tax)
