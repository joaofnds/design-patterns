# What is the Proxy Design Pattern?
#
# - Provide a class which will limit access to another class
# - You may do this for security reasons, because an Object is
#   intensive to create, or is accessed from a remote location
#
# @see http://www.newthinktank.com/2012/10/proxy-design-pattern-tutorial/

## code from state pattern tutorial
require 'forwardable'

class ATMState
  def insert_card
    raise NotImplementedError
  end

  def eject_card
    raise NotImplementedError
  end

  def insert_pin(_pin)
    raise NotImplementedError
  end

  def request_cash(_amount)
    raise NotImplementedError
  end
end

class ATMMachine
  extend Forwardable

  def_delegators :@state, :insert_card, :eject_card, :insert_pin, :request_cash

  attr_accessor :state,
                :cash,
                :has_card,
                :no_card,
                :has_pin,
                :has_correct_pin,
                :out_of_cash

  def initialize
    @has_card = HasCard.new(self)
    @no_card = NoCard.new(self)
    @has_pin = HasPin.new(self)
    @out_of_cash = NoCash.new(self)
    @state = @no_card
    @cash = 2000
    @has_correct_pin = false
  end
end

class HasCard < ATMState
  def initialize(atm)
    @atm = atm
  end

  def insert_card
    puts "you can't enter more than one card"
  end

  def eject_card
    puts 'card ejected'
    atm.state = atm.no_card
  end

  def insert_pin(pin)
    if pin == 1234
      puts 'correct PIN'
      atm.has_correct_pin = true
      atm.state = atm.has_pin
    else
      puts 'wrong PIN'
      atm.has_correct_pin = false
      puts 'card rejected'
      atm.state = atm.no_card
    end
  end

  def request_cash(_amount)
    puts 'enter PIN first'
  end

  private

  attr_reader :atm
end

class NoCard < ATMState
  def initialize(atm)
    @atm = atm
  end

  def insert_card
    puts 'please enter a PIN'
    atm.state = atm.has_card
  end

  def eject_card
    puts 'enter a card first'
  end

  def insert_pin(_pin)
    puts 'enter a card first'
  end

  def request_cash(_amount)
    puts 'enter a card first'
  end

  private

  attr_reader :atm
end

class HasPin < ATMState
  def initialize(atm)
    @atm = atm
  end

  def insert_card
    puts "You can't enter more than one card"
  end

  def eject_card
    puts 'card ejected'
    atm.state = atm.no_card
  end

  def insert_pin(_pin)
    puts 'already entered PIN'
  end

  def request_cash(amount)
    if !atm.has_correct_pin
      puts 'wrong pin'
      eject_card
    elsif amount > atm.cash
      puts "don't have that cash"
      eject_card
    else
      puts amount.to_s + ' is provided by the machine'
      atm.cash -= amount
      eject_card
      atm.state = atm.out_of_cash if atm.cash.zero?
    end
  end

  private

  attr_reader :atm
end

class NoCash < ATMState
  def initialize(atm)
    @atm = atm
  end

  def insert_card
    puts 'ATM out of cash'
  end

  def eject_card
    puts 'no card inserted'
  end

  def insert_pin(_pin)
    puts 'ATM out of cash'
  end

  def request_cash(_amount)
    puts 'ATM out of cash'
  end

  private

  attr_reader :atm
end
## code for this tutorial

# ATMProxy provides access do "safe" atm operations while deffering
# ATMMachine instantiation until really needed
class ATMProxy
  def state
    @atm ||= ATMMachine.new
    atm.state
  end

  def cash
    @atm ||= ATMMachine.new
    atm.cash
  end
end

################################################################################

atm_proxy = ATMProxy.new
puts atm_proxy.state
puts atm_proxy.cash
