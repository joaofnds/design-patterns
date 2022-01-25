/**
 * What is the Facade Pattern?
 *
 * - When you create a simplified interface that performs many
 *   other actions behind the scenes
 *
 * Ex:
 *   - Can I withdrawal $50 from the bank?
 *     - Check if the checking account is valid
 *     - Chceck if security code is valid
 *     - Chceck if fund are available
 *     - Make changes accordingly
 *
 * @see http://www.newthinktank.com/2012/09/facade-design-pattern-tutorial/
 */

class WelcomeToBank {
  public constructor() {
    console.log("Welcome to ABC Bank");
  }
}

class AccountNumberCheck {
  readonly accountNumber: number = 12345678;

  public accountActive(accountNumber: number): boolean {
    return accountNumber == this.accountNumber;
  }
}

class SecurityCodeCheck {
  readonly securityCode: number = 1234;

  public isCodeCorrect(securityCode: number): boolean {
    return securityCode == this.securityCode;
  }
}

class FundsCheck {
  #cashInAccount: number = 1000;

  get cashInAccount(): number {
    return this.#cashInAccount;
  }

  public decreaseCashInAccount(cashWithDraw: number): void {
    this.#cashInAccount -= cashWithDraw;
  }

  public increaseCashInAccount(cashDeposited: number): void {
    this.#cashInAccount += cashDeposited;
  }

  public haveEnoughMoney(cashToWithdrawal: number): boolean {
    return this.#cashInAccount >= cashToWithdrawal;
  }

  public makeDeposit(cashToDeposit: number): void {
    this.increaseCashInAccount(cashToDeposit);
  }
}

class BankAccountFacade {
  private readonly accChecker: AccountNumberCheck;
  private readonly codeChecker: SecurityCodeCheck;
  private readonly fundChecker: FundsCheck;
  private readonly bankWelcome: WelcomeToBank;

  public constructor(
    readonly accountNumber: number,
    readonly securityCode: number
  ) {
    this.bankWelcome = new WelcomeToBank();
    this.accChecker = new AccountNumberCheck();
    this.codeChecker = new SecurityCodeCheck();
    this.fundChecker = new FundsCheck();
  }

  public withdrawCash(cashToGet: number): void {
    if (
      this.accChecker.accountActive(this.accountNumber) &&
      this.codeChecker.isCodeCorrect(this.securityCode) &&
      this.fundChecker.haveEnoughMoney(cashToGet)
    ) {
      this.fundChecker.decreaseCashInAccount(cashToGet);
      console.log("transaction complete");
    } else {
      console.log("unable to complete transaction");
    }
  }

  public depositCash(cashToDeposit: number): void {
    if (
      this.accChecker.accountActive(this.accountNumber) &&
      this.codeChecker.isCodeCorrect(this.securityCode)
    ) {
      this.fundChecker.makeDeposit(cashToDeposit);
      console.log("transaction complete");
    } else {
      console.log("unable to complete transaction");
    }
  }
}

// ----------------------------------------------------------------------------

const accessingBank = new BankAccountFacade(12345678, 1234);
accessingBank.withdrawCash(500);
accessingBank.withdrawCash(501);
accessingBank.depositCash(1);
accessingBank.withdrawCash(501);
