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
  private name: string;

  public constructor() {
    console.log("Sheep is made");
  }

  public makeCopy(): Animal {
    console.log("Sheep is being made");
    const sheepObject: Sheep = Object.create(this);
    return sheepObject;
  }

  public setName(name: string): void {
    this.name = name;
  }

  public getName(): string {
    return this.name;
  }
}

class CloneFactory {
  public getClone(animalSample: Animal): Animal {
    return animalSample.makeCopy();
  }
}

//-------------------------------------------------

const animalMaker: CloneFactory = new CloneFactory();
const sally: Sheep = new Sheep();
sally.setName("sally");
const clonedSheep: Sheep = <Sheep>animalMaker.getClone(sally);
clonedSheep.setName("sally clone");
console.log(sally.getName());
console.log(clonedSheep.getName());
