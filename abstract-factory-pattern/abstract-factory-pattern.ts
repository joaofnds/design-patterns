/**
 * What is the Abstract Factory Pattern?
 *
 * - It is like a factory, but everything is encapsulated
 *   - The method that orders the subject
 *   - The factories that build the object
 *   - The final objects
 *   - The final objects contain objects that use Strategy Pattern
 *     - Composition: Object class fields are objects
 *
 * @see http://www.newthinktank.com/2012/09/abstract-factory-design-pattern/
 */

class ESWeapon { }
class ESEngine { }

class ESUFOGun extends ESWeapon { }
class ESUFOEngine extends ESEngine { }

class ESUFOBossGun extends ESWeapon { }
class ESUFOBossEngine extends ESEngine { }

abstract class EnemyShip {
  private name: string;

  // Newly defined objects that represent weapon & engine
  // These can be changed easily by assigning new parts
  // in UFOEnemyShipFactory or UFOBossEnemyShipFactory
  weapon: ESWeapon;
  engine: ESEngine;

  public getName(): string {
    return this.name;
  }
  public setName(name: string) {
    this.name = name;
  }

  abstract makeShip(): void;

  public followHeroShip(): void {
    console.log(this.getName(), "is following the hero at", this.engine);
  }

  public displayEnemyShip(): void {
    console.log(this.getName(), "is on the screen");
  }

  public enemyShipShoots(): void {
    console.log(this.getName, "attacks and does", this.weapon);
  }
}

class UFOEnemyShip extends EnemyShip {
  shipFactory: EnemyShipFactory;

  constructor(shipFactory: EnemyShipFactory) {
    super();
    this.shipFactory = shipFactory;
  }

  makeShip(): void {
    console.log("making the ship", this.getName());
    this.weapon = this.shipFactory.addESGun();
    this.engine = this.shipFactory.addESEngine();
  }
}

class UFOBossEnemyShip extends EnemyShip {
  shipFactory: EnemyShipFactory;

  constructor(shipFactory: EnemyShipFactory) {
    super();
    this.shipFactory = shipFactory;
  }

  makeShip(): void {
    console.log("making the ship", this.getName());
    this.weapon = this.shipFactory.addESGun();
    this.engine = this.shipFactory.addESEngine();
  }
}

// With an Abstract Factory Pattern you won't
// just build ships, but also all of the components
// for the ships
// Here is where you define the parts that are required
// if an object wants to be an enemy ship
interface EnemyShipFactory {
  addESGun(): ESWeapon;
  addESEngine(): ESEngine;
}

class UFOEnemyShipFactory implements EnemyShipFactory {
  addESGun(): ESWeapon {
    return new ESUFOGun();
  }

  addESEngine(): ESEngine {
    return new ESUFOEngine();
  }
}

class UFOBossEnemyShipFactory implements EnemyShipFactory {
  addESGun(): ESWeapon {
    return new ESUFOBossGun();
  }

  addESEngine(): ESEngine {
    return new ESUFOBossEngine();
  }
}

abstract class EnemyShipBuilding {
  // This acts as an ordering mechanism for creating
  // EnemyShips that have a weapon, engine & name
  // & nothing else

  // The specific parts used for engine & weapon depend
  // upon the String that is passed to this method

  protected abstract makeEnemyShip(typeOfShip: string): EnemyShip;

  // When called a new EnemyShip is made. The specific parts
  // are based on the String entered. After the ship is made
  // we execute multiple methods in the EnemyShip Object
  public orderTheShip(typeOfShip: string): EnemyShip {
    const theEnemyShip: EnemyShip = this.makeEnemyShip(typeOfShip);

    theEnemyShip.makeShip();
    theEnemyShip.displayEnemyShip();
    theEnemyShip.followHeroShip();
    theEnemyShip.enemyShipShoots();

    return theEnemyShip;
  }
}

class UFOEnemyShipBuilding extends EnemyShipBuilding {
  protected makeEnemyShip(typeOfShip: string): EnemyShip {
    let theEnemyShip: EnemyShip = null;

    // If UFO was sent grab use the factory that knows
    // what types of weapons and engines a regular UFO
    // needs. The EnemyShip object is returned & given a name
    if (typeOfShip === "UFO") {
      const shipPartsFactory: EnemyShipFactory = new UFOEnemyShipFactory();
      theEnemyShip = new UFOEnemyShip(shipPartsFactory);
      theEnemyShip.setName("UFO Grunt Ship");
    } else if (typeOfShip === "UFO BOSS") {
      // If UFO BOSS was sent grab use the factory that knows
      // what types of weapons and engines a Boss UFO
      // needs. The EnemyShip object is returned & given a name

      const shipPartsFactory: EnemyShipFactory = new UFOBossEnemyShipFactory();
      theEnemyShip = new UFOEnemyShip(shipPartsFactory);
      theEnemyShip.setName("UFO Boss Ship");
    }

    return theEnemyShip;
  }
}

// ----------------------------------------------------------

const MakeUFOs: EnemyShipBuilding = new UFOEnemyShipBuilding();

const theGrunt: EnemyShip = MakeUFOs.orderTheShip("UFO");
console.log(theGrunt);

const theBoss: EnemyShip = MakeUFOs.orderTheShip("UFO BOSS");
console.log(theBoss);
