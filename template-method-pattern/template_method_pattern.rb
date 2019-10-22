# What is the Template Method Design Pattern?
#
# - Used to create a group of subclasses that have to execute
#   a similar group of method
# - You create an abstract class that contains a method called
#   the Template Method
# - The Template Method contains a series of method calls that
#   every subclass object will call
# - The subclass object can override some of the method calls
#
# @see http://www.newthinktank.com/2012/10/template-method-design-pattern-tutorial/

class Hoagie
  def make_sandwich
    cut_bun
    add_meat if customer_wants_meat
    add_cheese if customer_wants_cheese
    add_vegetables if customer_wants_vegetables
    add_condiments if customer_wants_condiments
    wrap_the_hoagie
  end

  def cut_bun
    puts 'the hoagie is cut'
  end

  def wrap_the_hoagie
    puts 'wrap the hoagie'
  end

  def customer_wants_meat
    true
  end
  
  def customer_wants_cheese
    true
  end
  
  def customer_wants_vegetables
    true
  end
  
  def customer_wants_condiments
    true
  end
end

class ItalianHoagie < Hoagie
  @@meat = %w[salami pepperoni capicola\ ham]
  @@cheese = %w[provolone]
  @@veggies = %w[lettuce tomatoes onions sweet\ peppers]
  @@condiments = %w[oil vinegar]

  def add_meat
    print 'adding meat: '
    @@meat.each { |m| print m + ', ' }
    print "\n"
  end

  def add_cheese
    print 'adding cheese: '
    @@cheese.each { |c| print c + ', ' }
    print "\n"
  end

  def add_vegetables
    print 'adding veggies: '
    @@veggies.each { |v| print v + ', ' }
    print "\n"
  end

  def add_condiments
    print 'adding condiments: '
    @@condiments.each { |c| print c + ', ' }
    print "\n"
  end
end

class VeggieHoagie < Hoagie
  @@veggies = %w[lettuce tomatoes onions sweet\ peppers]
  @@condiments = %w[oil vinegar]

  def add_vegetables
    print 'adding veggies: '
    @@veggies.each { |v| print v + ', ' }
    print "\n"
  end

  def add_condiments
    print 'adding condiments: '
    @@condiments.each { |c| print c + ', ' }
    print "\n"
  end

  def customer_wants_meat; false end
  def customer_wants_cheese; false end
end

################################################################################

puts '##########################'
puts '##### Italian Hoagie #####'
puts '##########################'
italian_hoagie = ItalianHoagie.new
italian_hoagie.make_sandwich

puts ''
puts '#########################'
puts '##### Veggie Hoagie #####'
puts '#########################'
veggie_hoagie = VeggieHoagie.new
veggie_hoagie.make_sandwich
