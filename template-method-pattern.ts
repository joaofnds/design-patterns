/**
 * What is the Template Method Design Pattern?
 *
 * - Used to create a group of subclasses that have to execute
 *   a similar group of method
 * - You create an abstract class that contains a method called
 *   the Template Method
 * - The Template Method contains a series of method calls that
 *   every subclass object will call
 * - The subclass object can override some of the method calls
 *
 * @see http://www.newthinktank.com/2012/10/template-method-design-pattern-tutorial/
 */

abstract class Hoagie {
  makeSandwich(): void {
    this.cutBun();

    if (this.customerWantsMeat()) {
      this.addMeat();
    }

    if (this.customerWantsCheese()) {
      this.addCheese();
    }

    if (this.customerWantsVegetables()) {
      this.addVegetables();
    }

    if (this.customerWantsCondiments()) {
      this.addCondiments();
    }
  }

  public cutBun(): void {
    console.log("The Hoagie is cut");
  }

  customerWantsMeat(): boolean {
    return true;
  }
  customerWantsCheese(): boolean {
    return true;
  }
  customerWantsVegetables(): boolean {
    return true;
  }
  customerWantsCondiments(): boolean {
    return true;
  }

  abstract addMeat(): void;
  abstract addCheese(): void;
  abstract addVegetables(): void;
  abstract addCondiments(): void;

  public wrapTheHoagie(): void {
    console.log("Wrap the hoagie");
  }
}

class ItalianHoagie extends Hoagie {
  meatUsed: Array<String> = ["Salami", "Pepperoni", "Capicola Ham"];
  cheeseUsed: Array<String> = ["Provolone"];
  veggiesUsed: Array<String> = [
    "Lettuce",
    "Tomatoes",
    "Onions",
    "Sweet Peppers"
  ];
  condimentsUsed: Array<String> = ["Oil", "Vinegar"];

  addMeat(): void {
    console.log("Adding the meat:", ...this.meatUsed);
  }
  addCheese(): void {
    console.log("Adding the cheese:", ...this.cheeseUsed);
  }
  addVegetables(): void {
    console.log("Adding the veggies:", ...this.veggiesUsed);
  }
  addCondiments(): void {
    console.log("Adding the condiments:", ...this.condimentsUsed);
  }
}

class VeggieHoagie extends Hoagie {
  veggiesUsed: Array<String> = [
    "Lettuce",
    "Tomatoes",
    "Onions",
    "Sweet Peppers"
  ];
  condimentsUsed: Array<String> = ["Oil", "Vinegar"];

  customerWantsMeat(): boolean {
    return false;
  }

  customerWantsCheese(): boolean {
    return false;
  }

  addMeat(): void { }
  addCheese(): void { }
  addVegetables(): void {
    console.log("Adding the veggies:", ...this.veggiesUsed);
  }
  addCondiments(): void {
    console.log("Adding the condiments:", ...this.condimentsUsed);
  }
}

//----------------------------------------------------------------------

const cust12Hoagie: Hoagie = new ItalianHoagie();
cust12Hoagie.makeSandwich();

const cust13Hoagie: Hoagie = new VeggieHoagie();
cust13Hoagie.makeSandwich();
