class WelcomeToBank {
  public constructor() {
    console.log("Welcome to ABC Bank");
  }
}

class AccountNumberCheck {
  private accountNumber: number = 12345678;

  public getAccountNumber(): number {
    return this.accountNumber;
  }

  public accountActive(accountNumber: number): boolean {
    return accountNumber == this.getAccountNumber();
  }
}

class SecurityCodeCheck {
  private securityCode: number = 1234;

  public getSecurityCode(): number {
    return this.securityCode;
  }

  public isCodeCorrect(securityCode: number): boolean {
    return securityCode == this.getSecurityCode();
  }
}

class FundsCheck {
  private cashInAccount: number = 1000;

  public getCashInAccount(): number {
    return this.cashInAccount;
  }

  public decreaseCashInAccount(cashWithDraw: number): void {
    this.cashInAccount -= cashWithDraw;
  }

  public increaseCashInAccount(cashDeposited: number): void {
    this.cashInAccount += cashDeposited;
  }

  public haveEnoughMoney(cashToWithdrawal: number): boolean {
    return cashToWithdrawal > this.getCashInAccount();
  }

  public makeDeposit(cashToDeposit: number): void {
    this.increaseCashInAccount(cashToDeposit);
  }
}

class BankAccountFacade {
  private accountNumber: number;
  private securityCode: number;
  private accChecker: AccountNumberCheck;
  private codeChecker: SecurityCodeCheck;
  private fundChecker: FundsCheck;

  private bankWelcome: WelcomeToBank;

  public constructor(newAccountNumber: number, newSecurityCode: number) {
    this.accountNumber = newAccountNumber;
    this.securityCode = newSecurityCode;

    this.bankWelcome = new WelcomeToBank();
    this.accChecker = new AccountNumberCheck();
    this.codeChecker = new SecurityCodeCheck();
    this.fundChecker = new FundsCheck();
  }

  public getAccountNumber(): number {
    return this.accountNumber;
  }

  public getSecurityCode(): number {
    return this.securityCode;
  }

  public withdrawCash(cashToGet: number): void {
    if (
      this.accChecker.accountActive(this.getAccountNumber()) &&
      this.codeChecker.isCodeCorrect(this.getSecurityCode()) &&
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
      this.accChecker.accountActive(this.getAccountNumber()) &&
      this.codeChecker.isCodeCorrect(this.getSecurityCode())
    ) {
      this.fundChecker.makeDeposit(cashToDeposit);
      console.log("transaction complete");
    } else {
      console.log("unable to complete transaction");
    }
  }
}

//-----------------------------------------------------------
const accessingBank: BankAccountFacade = new BankAccountFacade(12345678, 1234);
accessingBank.withdrawCash(5000);
accessingBank.withdrawCash(90000);
accessingBank.depositCash(20000);
