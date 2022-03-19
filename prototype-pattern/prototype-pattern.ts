/**
 * What is the Prototype Pattern?
 *
 * - Creating new objects by cloning other objects
 * - Allows for adding of any subclass instance of a known super
 *   class at run time
 * - When there are numerous potential classes that you want to
 *   only use if needed at runtime
 * - Reduces the need for creating subclasses
 *
 * @see http://www.newthinktank.com/2012/09/prototype-design-pattern-tutorial/
 */

interface Animal {
  makeCopy(): Animal;
}

class Sheep implements Animal {
  name: string;

  public constructor() {
    console.log("Sheep is made");
  }

  makeCopy(): Animal {
    console.log("Sheep is being made");
    const sheepObject: Sheep = Object.create(this);
    return sheepObject;
  }
}

class CloneFactory {
  getClone(animalSample: Animal): Animal {
    return animalSample.makeCopy();
  }
}

//-------------------------------------------------

const animalMaker = new CloneFactory();
const sally = new Sheep();
sally.name = "sally";

const clonedSheep = <Sheep>animalMaker.getClone(sally);
clonedSheep.name = "sally clone";

console.log(sally.name);
console.log(clonedSheep.name);
