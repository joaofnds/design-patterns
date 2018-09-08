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
