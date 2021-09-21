/**
 * What is the Adapter Pattern?
 *
 * - Allows two incomplatible interfaces to work togheter
 * - Used when the client expects a (target) interface
 * - The adapter class allows the use of the available interface and
 *   the Target interface
 * - Any class can work togheter as long as the Adapter solves the
 *   issue that all classes must implement every method by the shared interface
 *
 * @see http://www.newthinktank.com/2012/09/adapter-design-pattern-tutorial/
 */

const rand = (n: number) => Math.round(Math.random() * n);

interface EnemyAttacker {
  fireWeapon(): void;
  driveForward(): void;
  assignDriver(driverName: string): void;
}

// Target
class EnemyTank implements EnemyAttacker {
  public fireWeapon() {
    const attackDamage: number = rand(10);
    console.log(`Enemy Tank Does ${attackDamage} damage`);
  }

  public driveForward() {
    const movement: number = rand(5);
    console.log(`Enemy tank moves ${movement} spaces`);
  }

  public assignDriver(driverName: string) {
    console.log(`${driverName} is driving the tank`);
  }
}

// Adaptee
class EnemyRobot {
  public smashWithHands() {
    const attackDamage: number = rand(10);
    console.log(`Enemy robot causes ${attackDamage} damage with its hands`);
  }

  public walkForward() {
    const movement = rand(5);
    console.log(`Enemy Robot Walks Forward ${movement} spaces`);
  }

  public reactToHuman(humanName: string) {
    console.log(`Enemy Robot Tramps on ${humanName}`);
  }
}

class EnemyRobotAdapter implements EnemyAttacker {
  private enemyRobot: EnemyRobot;

  public constructor(enemyRobot: EnemyRobot) {
    this.enemyRobot = enemyRobot;
  }

  public fireWeapon(): void {
    this.enemyRobot.smashWithHands();
  }

  public driveForward(): void {
    this.enemyRobot.walkForward();
  }

  public assignDriver(driverName: string): void {
    this.enemyRobot.reactToHuman(driverName);
  }
}

//---------------------------------------------------------------------------

const rx7Tank: EnemyTank = new EnemyTank();
const fredTheRobot: EnemyRobot = new EnemyRobot();
const robotAdapter: EnemyRobotAdapter = new EnemyRobotAdapter(fredTheRobot);

console.log("The Robot");
fredTheRobot.reactToHuman("João");
fredTheRobot.walkForward();
fredTheRobot.smashWithHands();

console.log("\nThe enemy tank");
rx7Tank.assignDriver("Vitor");
rx7Tank.driveForward();
rx7Tank.fireWeapon();

console.log("\nThe robot with adapter");
robotAdapter.assignDriver("Fernandes");
robotAdapter.driveForward();
robotAdapter.fireWeapon();
