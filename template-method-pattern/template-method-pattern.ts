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
  abstract addMeat(): void;
  abstract addCheese(): void;
  abstract addVegetables(): void;
  abstract addCondiments(): void;

  makeSandwich() {
    this.cutBun();

    if (this.customerWantsMeat()) this.addMeat();
    if (this.customerWantsCheese()) this.addCheese();
    if (this.customerWantsVegetables()) this.addVegetables();
    if (this.customerWantsCondiments()) this.addCondiments();
  }

  cutBun() {
    console.log("The Hoagie is cut");
  }

  customerWantsMeat() {
    return true;
  }

  customerWantsCheese() {
    return true;
  }

  customerWantsVegetables() {
    return true;
  }

  customerWantsCondiments() {
    return true;
  }

  wrapTheHoagie() {
    console.log("Wrap the hoagie");
  }
}

class ItalianHoagie extends Hoagie {
  meats = ["Salami", "Pepperoni", "Capicola Ham"];
  cheese = ["Provolone"];
  veggies = ["Lettuce", "Tomatoes", "Onions", "Sweet Peppers"];
  condiments = ["Oil", "Vinegar"];

  addMeat() {
    console.log("Adding the meat:", ...this.meats);
  }

  addCheese() {
    console.log("Adding the cheese:", ...this.cheese);
  }

  addVegetables() {
    console.log("Adding the veggies:", ...this.veggies);
  }

  addCondiments() {
    console.log("Adding the condiments:", ...this.condiments);
  }
}

class VeggieHoagie extends Hoagie {
  veggies = ["Lettuce", "Tomatoes", "Onions", "Sweet Peppers"];
  condiments = ["Oil", "Vinegar"];

  customerWantsMeat() {
    return false;
  }

  customerWantsCheese() {
    return false;
  }

  addMeat() {}
  addCheese() {}

  addVegetables() {
    console.log("Adding the veggies:", ...this.veggies);
  }

  addCondiments() {
    console.log("Adding the condiments:", ...this.condiments);
  }
}

//----------------------------------------------------------------------

const customer12Hoagie: Hoagie = new ItalianHoagie();
customer12Hoagie.makeSandwich();

const customer13Hoagie: Hoagie = new VeggieHoagie();
customer13Hoagie.makeSandwich();
