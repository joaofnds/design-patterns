=begin
What is the Decorator Pattern?

- The Decorator allows you to modify an object dynamically
- You would use it when you want the capabilities of inheritance with
  subclasses, but you need to add functionality at run time
- It is more flexible than inheritance
- Simplifies code because you add functionality using many simple classes
- Rather then rewrite old code, you can extend with new code

@see http://www.newthinktank.com/2012/09/decorator-design-pattern-tutorial/
=end

class Pizza
  attr_reader :description, :cost
end

class PlainPizza < Pizza
  def initialize
    @description = 'Thin Dough'
    @cost = 400
  end
end

class ToppingDecorator < Pizza
  attr_reader :pizza

  def initialize(pizza)
    @pizza = pizza
  end

  def description
    pizza.description
  end

  def cost
    pizza.cost
  end
end

class Mozzarella < ToppingDecorator
  def initialize(pizza)
    super(pizza)
    puts 'Adding dough'
    puts 'Adding Mozzarella'
  end

  def description
    pizza.description + ', Mozzarella'
  end

  def cost
    pizza.cost + 0.50
  end
end

class TomatoSauce < ToppingDecorator
  def initialize(pizza)
    super(pizza)
    puts 'Adding Sauce'
  end

  def description
    pizza.description + ', Tomato Sauce'
  end

  def cost
    pizza.cost + 0.35
  end
end

################################################################################

basic_pizza = TomatoSauce.new(Mozzarella.new(PlainPizza.new))
puts "Ingridients: #{basic_pizza.description}"
puts "Cost:        #{basic_pizza.cost}"
