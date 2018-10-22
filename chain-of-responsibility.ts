/**
 * What is the chain of responsibility design pattern?
 *
 * - This pattern send data to an object and if that object can't use it,
 * it sends it to any number of other objects that may be able to use it
 *    - Create 4 objects that can either add, subtract, multiply, or divide
 *    - Send 2 numbers and a command and allow these 4 object to decide
 *      which can handle the requested calculation
 *
 * @see http://www.newthinktank.com/2012/10/chain-of-responsibility-design-pattern-tutorial/
 */

interface Chain {
  setNextChain(nextChain: Chain): void;
  calculate(request: Numbers): void;
}

class Numbers {
  private number1: number;
  private number2: number;

  private calculationWanted: string;

  public constructor(
    newNumber1: number,
    newNumber2: number,
    calcWanted: string
  ) {
    this.number1 = newNumber1;
    this.number2 = newNumber2;
    this.calculationWanted = calcWanted;
  }

  public getNumber1(): number {
    return this.number1;
  }

  public getNumber2(): number {
    return this.number2;
  }

  public getCalcWanted(): string {
    return this.calculationWanted;
  }
}

class AddNumbers implements Chain {
  private nextInChain: Chain;

  setNextChain(nextChain: Chain): void {
    this.nextInChain = nextChain;
  }
  calculate(request: Numbers): void {
    if (request.getCalcWanted() == "add") {
      console.log(
        `${request.getNumber1()} + ${request.getNumber2()} = ${request.getNumber1() +
        request.getNumber2()}`
      );
    } else {
      this.nextInChain.calculate(request);
    }
  }
}

class SubtractNumbers implements Chain {
  private nextInChain: Chain;

  setNextChain(nextChain: Chain): void {
    this.nextInChain = nextChain;
  }
  calculate(request: Numbers): void {
    if (request.getCalcWanted() == "subtract") {
      console.log(
        `${request.getNumber1()} - ${request.getNumber2()} = ${request.getNumber1() -
        request.getNumber2()}`
      );
    } else {
      this.nextInChain.calculate(request);
    }
  }
}

class MultiplyNumbers implements Chain {
  private nextInChain: Chain;

  setNextChain(nextChain: Chain): void {
    this.nextInChain = nextChain;
  }
  calculate(request: Numbers): void {
    if (request.getCalcWanted() == "multiply") {
      console.log(
        `${request.getNumber1()} * ${request.getNumber2()} = ${request.getNumber1() *
        request.getNumber2()}`
      );
    } else {
      this.nextInChain.calculate(request);
    }
  }
}

class DivideNumbers implements Chain {
  private nextInChain: Chain;

  setNextChain(nextChain: Chain): void {
    this.nextInChain = nextChain;
  }
  calculate(request: Numbers): void {
    if (request.getCalcWanted() == "divide") {
      console.log(
        `${request.getNumber1()} / ${request.getNumber2()} = ${request.getNumber1() /
        request.getNumber2()}`
      );
    } else {
      this.nextInChain.calculate(request);
    }
  }
}

//--------------------------------------------------------
const chainCalc1: Chain = new AddNumbers();
const chainCalc2: Chain = new SubtractNumbers();
const chainCalc3: Chain = new MultiplyNumbers();
const chainCalc4: Chain = new DivideNumbers();

chainCalc1.setNextChain(chainCalc2);
chainCalc2.setNextChain(chainCalc3);
chainCalc3.setNextChain(chainCalc4);

const request: Numbers = new Numbers(4, 2, "subtract");

chainCalc1.calculate(request);
