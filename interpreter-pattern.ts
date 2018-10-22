/**
 * What is the interpreter design pattern?
 *
 * - It is used to convert one representation of data into another
 * - The Context contains the information that will be interpreted
 * - The Expression is an abstract class that defines all the methods
 *   needed to perform the different conversions
 * - The Terminal or Concrete Expressions provide specific conversions on
 *   different types of data
 *
 * @see http://www.newthinktank.com/2012/10/interpreter-design-pattern-tutorial/
 */

class ConversionContext {
  private conversionQuestion: string = "";
  private conversionResponse: string = "";
  private fromConversion: string = "";
  private toConversion: string = "";
  private partsOfQuestion: Array<string>;

  private quantity: number;

  public constructor(input: string) {
    this.conversionQuestion = input;
    this.partsOfQuestion = input.split(" ");
    this.fromConversion = this.getCapitalized(
      this.sanitize(this.partsOfQuestion[1])
    );
    this.toConversion = this.sanitize(this.partsOfQuestion[3]).toLowerCase();
    this.quantity = Number(this.partsOfQuestion[0]);
    this.conversionResponse = `${this.partsOfQuestion[0]} ${
      this.partsOfQuestion[1]
      } equals `;
  }

  public getInput(): string {
    return this.conversionQuestion;
  }

  public getFromConversion(): string {
    return this.fromConversion;
  }

  public getToConversion(): string {
    return this.toConversion;
  }

  public getResponse(): string {
    return this.conversionResponse;
  }

  public getQuantity(): number {
    return this.quantity;
  }

  private getCapitalized(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }

  private sanitize(str: string): string {
    const lowerStr = str.toLowerCase();
    if (lowerStr.charAt(str.length - 1) === "s") {
      return lowerStr;
    } else {
      return lowerStr + "s";
    }
  }
}

abstract class Expression {
  abstract bits(quantity: number): string;
  abstract nibbles(quantity: number): string;
  abstract bytes(quantity: number): string;
}

class Bit extends Expression {
  bits(quantity: number): string {
    return String(quantity);
  }
  nibbles(quantity: number): string {
    return String(Math.floor(quantity / 4));
  }
  bytes(quantity: number): string {
    return String(Math.floor(quantity / 8));
  }
}

class Nibble extends Expression {
  bits(quantity: number): string {
    return String(quantity * 4);
  }
  nibbles(quantity: number): string {
    return String(quantity);
  }
  bytes(quantity: number): string {
    return String(Math.floor(quantity / 2));
  }
}

class Byte extends Expression {
  bits(quantity: number): string {
    return String(quantity * 8);
  }
  nibbles(quantity: number): string {
    return String(quantity * 2);
  }
  bytes(quantity: number): string {
    return String(quantity);
  }
}

//---------------------------------------------
const getClassFromInput = (input): Bit | Nibble | Byte | null => {
  switch (input) {
    case "Bits":
      return new Bit();
    case "Bytes":
      return new Byte();
    case "Nibbles":
      return new Nibble();
    default:
      return null;
  }
};

const questionAsked = "8 bits to bytes";
const question: ConversionContext = new ConversionContext(questionAsked);

const fromConversion: string = question.getFromConversion();
const toConversion: string = question.getToConversion();
const quantity: number = question.getQuantity();

const convertFrom: Expression = getClassFromInput(fromConversion);
const result = convertFrom[toConversion](quantity);
console.log(`${quantity} ${fromConversion} is ${result} ${toConversion}`);
