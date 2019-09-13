# frozen_string_literal: true

# When to use the Observer Pattern
#
# - when you need many other object to receive an update when
#   another object changes
# Ex:
#   - Stock market with thousands of stocks need to send updates to
#     objects representing individual stocks
#   - The Subject(publisher) sends many stocks to the observers
#   - The Observers(subscribers) takes the ones they want and use them
#
# @see http://www.newthinktank.com/2012/08/observer-design-pattern-tutorial/

class StockGrabber
  @observers = []

  attr_reader :observers, :stocks

  def initialize
    @observers = []
    @stocks = {}
  end

  # Subject Interface

  def register(observer)
    observers << observer
  end

  def unregister(observer)
    observers.delete(observer)
  end

  def notify_observers
    observers.each do |observer|
      observer.update(stocks)
    end
  end

  # StockGrabber specific methods

  def add_stock(name, price)
    stocks[name] = price unless stocks.key? name
    notify_observers
  end

  def remove_stock(name)
    stocks.delete(name)
    notify_observers
  end

  def update_stock(name, price)
    stocks[name] = price
    notify_observers
  end
end

class StockObserver
  @@observer_id_tracker = 0

  attr_reader :stocks

  def initialize(stock_grabber, stocks = {})
    @stocks = stocks
    @stock_grabber = stock_grabber
    @observer_id = @@observer_id_tracker += 1
    @stock_grabber.register(self)
  end

  def update(stocks)
    @stocks = stocks
    puts self
  end

  def to_s
    stock_prices_s = stocks.map { |name, price| "#{name}: #{price}" }.join("\n")
    <<~STRING
      #{'#' * 10} Observer: #{@observer_id} #{'#' * 10}
      #{stock_prices_s}
      #{'#' * 33}
    STRING
  end
end

################################################################################

stock_grabber = StockGrabber.new
observer_one = StockObserver.new(stock_grabber)

stock_grabber.add_stock('IBM', 10)
stock_grabber.add_stock('AAPL', 10)

_ = StockObserver.new(stock_grabber)

stock_grabber.add_stock('GOOG', 10)
stock_grabber.update_stock('GOOG', 20)

stock_grabber.unregister(observer_one)

stock_grabber.remove_stock('AAPL')
