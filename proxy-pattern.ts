// Code from previous tutorial (state pattern)
interface ATMState {
  insertCard(): void;
  ejectCard(): void;
  insertPin(pinEntered: number): void;
  requestCash(cashToWithdraw: number): void;
}

class HasCard implements ATMState {
  atmMachine: ATMMachine;

  public constructor(newATMMachine: ATMMachine) {
    this.atmMachine = newATMMachine;
  }

  insertCard(): void {
    console.error("Card tray full");
  }

  ejectCard(): void {
    this.atmMachine.setATMState(this.atmMachine.getNoCardState());
    console.log("Card ejected");
  }

  insertPin(pinEntered: number): void {
    if (pinEntered === 1234) {
      this.atmMachine.correctPinEntered = true;
      this.atmMachine.setATMState(this.atmMachine.getHasPin());
    } else {
      console.error("Wrong PIN");
      this.atmMachine.correctPinEntered = false;
      this.ejectCard();
    }
  }

  requestCash(cashToWithdraw: number): void {
    console.error("Enter PIN first");
  }
}

class NoCard implements ATMState {
  atmMachine: ATMMachine;

  public constructor(newATMMachine: ATMMachine) {
    this.atmMachine = newATMMachine;
  }

  insertCard(): void {
    console.log("Please enter a PIN");
    this.atmMachine.setATMState(this.atmMachine.getHasCardState());
  }

  ejectCard(): void {
    console.error("Enter a card first");
  }

  insertPin(pinEntered: number): void {
    console.error("Enter a card first");
  }

  requestCash(cashToWithdraw: number): void {
    console.error("Enter a card first");
  }
}

class HasPin implements ATMState {
  atmMachine: ATMMachine;

  public constructor(newATMMachine: ATMMachine) {
    this.atmMachine = newATMMachine;
  }

  insertCard(): void {
    console.error("Card tray full");
  }

  ejectCard(): void {
    this.atmMachine.setATMState(this.atmMachine.getNoCardState());
    console.log("Card ejected");
  }

  insertPin(pinEntered: number): void {
    console.log("Already entered PIN");
  }

  requestCash(cashToWithdraw: number): void {
    if (cashToWithdraw > this.atmMachine.cashInMachine) {
      console.error("Don't have that cash");
    } else {
      console.log(`${cashToWithdraw} is provided by the machine`);
      this.atmMachine.setCashInMachine(
        this.atmMachine.cashInMachine - cashToWithdraw
      );

      if (this.atmMachine.cashInMachine <= 0) {
        this.atmMachine.setATMState(this.atmMachine.getNoCashState());
      }
    }

    this.ejectCard();
  }
}

class NoCash implements ATMState {
  atmMachine: ATMMachine;

  public constructor(newATMMachine: ATMMachine) {
    this.atmMachine = newATMMachine;
  }

  insertCard(): void {
    console.error("No money");
  }

  ejectCard(): void {
    console.error("No money");
    console.error("You didn't enter a card");
  }

  insertPin(pinEntered: number): void {
    console.error("No money");
  }

  requestCash(cashToWithdraw: number): void {
    console.error("No money");
  }
}

class ATMMachine {
  hasCard: ATMState;
  noCard: ATMState;
  hasCorrectPin: ATMState;
  atmOutOfMoney: ATMState;

  atmState: ATMState;

  cashInMachine: number = 2000;
  correctPinEntered: boolean = false;

  public constructor() {
    this.hasCard = new HasCard(this);
    this.noCard = new NoCard(this);
    this.hasCorrectPin = new HasPin(this);
    this.atmOutOfMoney = new NoCash(this);

    this.atmState = this.noCard;

    if (this.cashInMachine < 0) {
      this.atmState = this.atmOutOfMoney;
    }
  }

  public setATMState(newATMState: ATMState): void {
    this.atmState = newATMState;
  }

  public setCashInMachine(newCashInMachine: number): void {
    this.cashInMachine = newCashInMachine;
  }

  public insertCard(): void {
    this.atmState.insertCard();
  }

  public ejectCard(): void {
    this.atmState.ejectCard();
  }

  public requestCash(cashToWithdraw: number): void {
    this.atmState.requestCash(cashToWithdraw);
  }

  public insertPin(pinEntered: number): void {
    this.atmState.insertPin(pinEntered);
  }

  public getHasCardState(): ATMState {
    return this.hasCard;
  }

  public getNoCardState(): ATMState {
    return this.noCard;
  }

  public getHasPin(): ATMState {
    return this.hasCorrectPin;
  }

  public getNoCashState(): ATMState {
    return this.atmOutOfMoney;
  }
}

//-------------------------------------------------------------------------
// Actual proxy pattern code
interface GetATMData {
  getATMData(): ATMState;
  getCashInMachine(): number;
}

class NewATM extends ATMMachine implements GetATMData {
  getATMData(): ATMState {
    return this.atmState;
  }
  getCashInMachine(): number {
    return this.cashInMachine;
  }
}

class ATMProxy implements GetATMData {
  getATMData(): ATMState {
    const realATMMachine: NewATM = new NewATM();
    return realATMMachine.getATMData();
  }
  getCashInMachine(): number {
    const realATMMachine: NewATM = new NewATM();
    return realATMMachine.getCashInMachine();
  }
}

//----------------------------------------
const machine: GetATMData = new NewATM();
const proxy: GetATMData = new ATMProxy();

console.log(proxy.getATMData());
console.log(proxy.getCashInMachine());
