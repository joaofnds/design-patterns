=begin
What is the Facade Pattern?

- When you create a simplified interface that performs many
  other actions behind the scenes

Ex:
  - Can I withdrawal $50 from the bank?
  - Check if the checking account is valid
  - Chceck if security code is valid
  - Chceck if fund are available
  - Make changes accordingly

@see http://www.newthinktank.com/2012/09/facade-design-pattern-tutorial/
=end

class WelcomeBank
  def initialize
    puts 'Welcome to ABC Bank'
  end
end

class AccountNumberCheck
  def account_active?(account_number)
    account_number == 12345678
  end
end

class SecurityCodeCheck
  def correct_code?(security_code)
    security_code == 1234
  end
end

class FundsCheck
  def initialize
    @cash_in_account = 100000
  end

  def decrease_cash_in_account(cash_withdraw)
    @cash_in_account = @cash_in_account - cash_withdraw
  end

  def increase_cash_in_account(cash_deposit)
    @cash_in_account = @cash_in_account + cash_deposit
  end

  def enough_money?(cash_to_withdraw)
    if cash_to_withdraw > @cash_in_account
      puts "[error] you don't have enough money"
      puts "[error] current balance: #{@cash_in_account}"
      false
    else
      decrease_cash_in_account(cash_to_withdraw)
      puts "[withdraw complete] current balance is: #{@cash_in_account}"
      true
    end
  end

  def make_deposit(cash_to_deposit)
    increase_cash_in_account(cash_to_deposit)
    puts "[deposit complete] current balance is: #{@cash_in_account}"
  end
end

class BankAccountFacade
  attr_reader :account_number,
              :security_code,
              :welcome_bank,
              :account_checker,
              :code_checker,
              :fund_checker

  def initialize(account_number, security_code)
    @account_number = account_number
    @security_code = security_code
    @welcome_bank = WelcomeBank.new
    @account_checker = AccountNumberCheck.new
    @code_checker = SecurityCodeCheck.new
    @fund_checker = FundsCheck.new
  end

  def withdraw_cash(cash)
    if account_checker.account_active?(account_number) &&
       code_checker.correct_code?(security_code) &&
       fund_checker.enough_money?(cash)
      puts 'Transaction complete'
    else
      puts 'Transaction failed'
    end
  end

  def deposit_cash(cash)
    if account_checker.account_active?(account_number) &&
       code_checker.correct_code?(security_code)
      fund_checker.make_deposit(cash)
      puts 'Transaction complete'
    else
      puts 'Transaction failed'
    end
  end
end

################################################################################

accessing_bank = BankAccountFacade.new(12345678, 1234)
accessing_bank.withdraw_cash(5000)
accessing_bank.withdraw_cash(90000)
accessing_bank.deposit_cash(20000)