/**
 * What is the Builder Pattern?
 *
 * - Pattern used to create objects made from a bunch of other objects
 *   - When you want to build an object made up from other objects
 *   - When you want the creation of these parts to be independent
 *     of the main object
 *   - Hide the creation of the parts from the client so both
 *     aren't dependent
 *   - The builder know the specifics and nobody else does
 *
 * @see http://www.newthinktank.com/2012/09/builder-design-pattern-tutorial/
 */

interface RobotPlan {
  setRobotHead(head: string): void;
  setRobotTorso(torso: string): void;
  setRobotArms(arms: string): void;
  setRobotLegs(legs: string): void;
}

class Robot implements RobotPlan {
  private robotHead: string;
  private robotTorso: string;
  private robotArms: string;
  private robotLegs: string;

  public getRobotHead(): string {
    return this.robotHead;
  }
  public setRobotHead(head: string): void {
    this.robotHead = head;
  }

  public getRobotTorso(): string {
    return this.robotTorso;
  }
  public setRobotTorso(torso: string): void {
    this.robotTorso = torso;
  }

  public getRobotArms(): string {
    return this.robotArms;
  }
  public setRobotArms(arms: string): void {
    this.robotArms = arms;
  }

  public getRobotLegs(): string {
    return this.robotLegs;
  }
  public setRobotLegs(legs: string): void {
    this.robotLegs = legs;
  }
}

interface RobotBuilder {
  buildRobotHead(): void;
  buildRobotTorso(): void;
  buildRobotArms(): void;
  buildRobotLegs(): void;
  getRobot(): Robot;
}

class OldRobotBuilder implements RobotBuilder {
  private robot: Robot;

  public constructor() {
    this.robot = new Robot();
  }

  public getRobot(): Robot {
    return this.robot;
  }

  public buildRobotHead(): void {
    this.robot.setRobotHead("Tin Head");
  }
  public buildRobotTorso(): void {
    this.robot.setRobotTorso("Tin Torso");
  }
  public buildRobotArms(): void {
    this.robot.setRobotArms("Tin Arms");
  }
  public buildRobotLegs(): void {
    this.robot.setRobotLegs("Tin Head");
  }
}

class RobotEngineer {
  private robotBuilder: RobotBuilder;

  public constructor(robotBuilder: RobotBuilder) {
    this.robotBuilder = robotBuilder;
  }

  public getRobot(): Robot {
    return this.robotBuilder.getRobot();
  }

  public makeRobot(): void {
    this.robotBuilder.buildRobotHead();
    this.robotBuilder.buildRobotTorso();
    this.robotBuilder.buildRobotArms();
    this.robotBuilder.buildRobotLegs();
  }
}

// -------------------
const oldStyleRobot: OldRobotBuilder = new OldRobotBuilder();
const robotEngineer: RobotEngineer = new RobotEngineer(oldStyleRobot);

robotEngineer.makeRobot();
const firstRobot: Robot = robotEngineer.getRobot();

console.log("robot head :", firstRobot.getRobotHead());
console.log("robot torso:", firstRobot.getRobotTorso());
console.log("robot arms :", firstRobot.getRobotArms());
console.log("robot legs :", firstRobot.getRobotLegs());
