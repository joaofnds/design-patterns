# What is the Mediator Design Pattern?
#
# - It is used to handle communication between related objects
# - All communication is handled by the Mediator and the Colleagues
#   don't need to know anything about each other
# - GOF: Allows loose coupling by encapsulating the way disparate
#   sets of objects interact and communicate with each other.
#   Allows for the actions of each object set to vary independently
#   of one another.
#
# @see http://www.newthinktank.com/2012/10/mediator-design-pattern-tutorial/

class StockOffer
  attr_reader :shares, :symbol, :colleague_code

  def initialize(shares, symbol, colleague_code)
    @shares = shares
    @symbol = symbol
    @colleague_code = colleague_code
  end
end

class Colleague
  attr_accessor :mediator, :code

  def initialize(mediator)
    @mediator = mediator
    @mediator.add_colleague self
  end

  def sale_offer(stock, shares)
    mediator.sale_offer(stock, shares, code)
  end

  def buy_offer(stock, shares)
    mediator.buy_offer(stock, shares, code)
  end
end

class GormanSlacks < Colleague
  def initialize(mediator)
    super
    puts 'Gorman Slacks singed up for the exchange'
  end
end

class JTPoorman < Colleague
  def initialize(mediator)
    super
    puts 'JT Poorman singed up for the exchange'
  end
end

class StockMediator
  attr_reader :colleagues, :buy_offers, :sale_offers, :code

  def initialize
    @code = 0
    @colleagues = []
    @buy_offers = []
    @sale_offers = []
  end

  def add_colleague(colleague)
    @colleagues << colleague
    colleague.code = @code
    @code += 1
  end

  def sale_offer(stock, shares, code)
    offer = StockOffer.new(shares, stock, code)
    @sale_offers << offer

    @buy_offers.each do |buy_offer|
      if offer_match? buy_offer, offer
        buy(buy_offer(offer))
        break
      end
    end
  end

  def buy_offer(stock, shares, code)
    offer = StockOffer.new(shares, stock, code)
    @buy_offers << offer

    @sale_offers.each do |sale_offer|
      buy(offer, sale_offer) if offer_match? sale_offer, offer
    end
  end

  private

  def offer_match?(offer1, offer2)
    offer1.symbol == offer2.symbol && offer1.shares == offer2.shares && offer1.colleague_code != offer2.colleague_code
  end

  def matching_offer(offer)
    @sale_offers.select { |o| offer_match? o, offer }.first
  end

  def buy(buy_offer, sale_offer)
    @sale_offers.delete sale_offer
    @buy_offers.delete buy_offer

    puts "#{sale_offer.shares} shares of #{sale_offer.symbol} sold to colleague of code #{buy_offer.colleague_code}"
  end
end

################################################################################

nyse = StockMediator.new
broker = GormanSlacks.new(nyse)
broker2 = JTPoorman.new(nyse)

broker.sale_offer('MSFT', 100)
broker.sale_offer('GOOG', 50)

broker2.sale_offer('NRG', 10)

broker.buy_offer('NRG', 10)
broker2.buy_offer('MSFT', 100)

puts "\nsale offers " + '=' * 15
puts nyse.sale_offers.map { |o| "#{o.symbol} - #{o.shares}" }
puts "\nbuy offers  " + '=' * 15
puts nyse.buy_offers.map { |o| "#{o.symbol} - #{o.shares}" }
