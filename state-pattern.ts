/**
 * What is the State Design Pattern?
 *
 * - Allows an object to alter its behavior when its internal state changes.
 *   The object will appear to change its class
 * - Context (account): Maintains an instance of a ConcreateState subclass
 *   that defines the current state
 * - State: Defines an interface for encapsulating the behavior associated
 *   with a particular state of the Context
 * - Concrete State: Each subclass implements a behavior associated with
 *   a state of Context
 *
 * @see http://www.newthinktank.com/2012/10/state-design-pattern-tutorial/
 */

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

const atmMachine: ATMMachine = new ATMMachine();
atmMachine.insertCard();
atmMachine.ejectCard();
atmMachine.insertCard();
atmMachine.insertPin(1234);
atmMachine.requestCash(2000);
atmMachine.insertCard();
atmMachine.insertPin(1234);
atmMachine.requestCash(2000);
