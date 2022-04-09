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
  flyingType: Flyer;

  tryToFly(): string {
    return this.flyingType.fly();
  }

  setFlyingAbility(flyingType: Flyer) {
    this.flyingType = flyingType;
  }
}

class Dog extends Animal {
  flyingType = new CantFly();
}

class Bird extends Animal {
  flyingType = new ItFlies();
}

interface Flyer {
  fly(): string;
}

class ItFlies implements Flyer {
  fly() {
    return "flying high";
  }
}

class CantFly implements Flyer {
  fly() {
    return "I can't fly";
  }
}

//------------------------------------------------------------------------------

const sparky = new Dog();
const tweety = new Bird();

console.log(`dog:  ${sparky.tryToFly()}`);
console.log(`bird: ${tweety.tryToFly()}`);

sparky.setFlyingAbility(new ItFlies());
console.log(`dog:  ${sparky.tryToFly()}`);
