/**
 * What is the Proxy Design Pattern?
 *
 * - Provide a class which will limit access to another class
 * - You may do this for security reasons, because an Object is
 *   intensive to create, or is accessed from a remote location
 *
 * @see http://www.newthinktank.com/2012/10/proxy-design-pattern-tutorial/
 */

// Code from previous tutorial (state pattern)
interface ATMState {
  insertCard(): void;
  ejectCard(): void;
  insertPin(pinEntered: number): void;
  requestCash(cashToWithdraw: number): void;
}

class HasCard implements ATMState {
  public constructor(private readonly machine: ATMMachine) {}

  insertCard() {
    console.error("Card tray full");
  }

  ejectCard() {
    this.machine.setATMState(this.machine.getNoCardState());
    console.log("Card ejected");
  }

  insertPin(pinEntered: number) {
    if (pinEntered === 1234) {
      this.machine.correctPinEntered = true;
      this.machine.setATMState(this.machine.getHasPin());
    } else {
      console.error("Wrong PIN");
      this.machine.correctPinEntered = false;
      this.ejectCard();
    }
  }

  requestCash(_cashToWithdraw: number) {
    console.error("Enter PIN first");
  }
}

class NoCard implements ATMState {
  public constructor(private readonly machine: ATMMachine) {}

  insertCard() {
    console.log("Please enter a PIN");
    this.machine.setATMState(this.machine.getHasCardState());
  }

  ejectCard() {
    console.error("Enter a card first");
  }

  insertPin(_pinEntered: number) {
    console.error("Enter a card first");
  }

  requestCash(_cashToWithdraw: number) {
    console.error("Enter a card first");
  }
}

class HasPin implements ATMState {
  public constructor(private readonly machine: ATMMachine) {}

  insertCard() {
    console.error("Card tray full");
  }

  ejectCard() {
    this.machine.setATMState(this.machine.getNoCardState());
    console.log("Card ejected");
  }

  insertPin(_pinEntered: number) {
    console.log("Already entered PIN");
  }

  requestCash(cashToWithdraw: number) {
    if (cashToWithdraw > this.machine.cashAvailable) {
      console.error("Don't have that cash");
    } else {
      console.log(`${cashToWithdraw} is provided by the machine`);
      this.machine.setCashAvailable(
        this.machine.cashAvailable - cashToWithdraw
      );

      if (this.machine.cashAvailable <= 0) {
        this.machine.setATMState(this.machine.getNoCashState());
      }
    }

    this.ejectCard();
  }
}

class NoCash implements ATMState {
  public constructor(_machine: ATMMachine) {}

  insertCard() {
    console.error("No money");
  }

  ejectCard() {
    console.error("No money");
    console.error("You didn't enter a card");
  }

  insertPin(_pinEntered: number) {
    console.error("No money");
  }

  requestCash(_cashToWithdraw: number) {
    console.error("No money");
  }
}

class ATMMachine {
  stateHasCard: ATMState;
  stateNoCard: ATMState;
  stateHasCorrectPin: ATMState;
  stateATMOutOfMoney: ATMState;

  currentState: ATMState;

  cashAvailable: number = 2000;
  correctPinEntered: boolean = false;

  public constructor() {
    this.stateHasCard = new HasCard(this);
    this.stateNoCard = new NoCard(this);
    this.stateHasCorrectPin = new HasPin(this);
    this.stateATMOutOfMoney = new NoCash(this);

    this.currentState = this.stateNoCard;

    if (this.cashAvailable < 0) {
      this.currentState = this.stateATMOutOfMoney;
    }
  }

  public setATMState(newState: ATMState) {
    this.currentState = newState;
  }

  public setCashAvailable(amount: number) {
    this.cashAvailable = amount;
  }

  public insertCard() {
    this.currentState.insertCard();
  }

  public ejectCard() {
    this.currentState.ejectCard();
  }

  public requestCash(cashToWithdraw: number) {
    this.currentState.requestCash(cashToWithdraw);
  }

  public insertPin(pin: number) {
    this.currentState.insertPin(pin);
  }

  public getHasCardState(): ATMState {
    return this.stateHasCard;
  }

  public getNoCardState(): ATMState {
    return this.stateNoCard;
  }

  public getHasPin(): ATMState {
    return this.stateHasCorrectPin;
  }

  public getNoCashState(): ATMState {
    return this.stateATMOutOfMoney;
  }
}

//-------------------------------------------------------------------------
// Actual proxy pattern code
interface GetATMData {
  getATMData(): ATMState;
  getCashAvailable(): number;
}

class NewATM extends ATMMachine implements GetATMData {
  getATMData() {
    return this.currentState;
  }

  getCashAvailable() {
    return this.cashAvailable;
  }
}

class ATMProxy implements GetATMData {
  getATMData() {
    return new NewATM().getATMData();
  }

  getCashAvailable() {
    return new NewATM().getCashAvailable();
  }
}

//----------------------------------------
const machine: GetATMData = new NewATM();
const proxy: GetATMData = new ATMProxy();

console.log(machine.getATMData());
console.log(machine.getCashAvailable());
console.log(proxy.getATMData());
console.log(proxy.getCashAvailable());
