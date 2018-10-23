/**
 * What is the strategy design pattern?
 *
 * - When you want to define a class that will have one behavior that is
 *   similar to other behaviors in a list
 * - When you need to use one of several behaviors dynamically
 * - You use this pattern if you need to dynamically change an
 *   algorithm used by an object at run time.
 * -  Allows you to eliminate code duplication
 * - Separates behavior from super and subclasses
 *
 * @see http://www.newthinktank.com/2012/08/strategy-design-pattern-tutorial/
 */

class Animal {
  public flyingType: Flys;

  public tryToFly(): string {
    return this.flyingType.fly();
  }

  public setFlyingAbility(newFlyType: Flys): void {
    this.flyingType = newFlyType;
  }
}

class Dog extends Animal {
  public constructor() {
    super();
    this.flyingType = new CantFly();
  }
}

class Bird extends Animal {
  public constructor() {
    super();
    this.flyingType = new ItFlys();
  }
}

interface Flys {
  fly(): string;
}

class ItFlys implements Flys {
  public fly(): string {
    return "flying high";
  }
}

class CantFly implements Flys {
  public fly(): string {
    return "I can't fly";
  }
}

//------------------------------------------------------------------------------

const sparky: Animal = new Dog();
const tweety: Animal = new Bird();

console.log(`dog : ${sparky.tryToFly()}`)
console.log(`bird: ${tweety.tryToFly()}`)

sparky.setFlyingAbility(new ItFlys());

console.log(`dog : ${sparky.tryToFly()}`)