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
  head: string;
  torso: string;
  arms: string;
  legs: string;
}

interface RobotBuilder {
  buildHead(): void;
  buildTorso(): void;
  buildArms(): void;
  buildLegs(): void;
  getRobot(): Robot;
}

class Robot implements RobotPlan {
  head: string = "";
  torso: string = "";
  arms: string = "";
  legs: string = "";
}

class OldRobotBuilder implements RobotBuilder {
  private robot: Robot;

  public constructor() {
    this.robot = new Robot();
  }

  public getRobot(): Robot {
    return this.robot;
  }

  public buildHead(): void {
    this.robot.head = "Tin Head";
  }

  public buildTorso(): void {
    this.robot.torso = "Tin Torso";
  }

  public buildArms(): void {
    this.robot.arms = "Blowtorch Arms";
  }

  public buildLegs(): void {
    this.robot.legs = "Roller Skates";
  }
}

class RobotEngineer {
  public constructor(private builder: RobotBuilder) {}

  public getRobot(): Robot {
    return this.builder.getRobot();
  }

  public makeRobot(): void {
    this.builder.buildHead();
    this.builder.buildTorso();
    this.builder.buildArms();
    this.builder.buildLegs();
  }
}

// -------------------------------------------------------------------------------

const oldStyleRobot: OldRobotBuilder = new OldRobotBuilder();
const robotEngineer: RobotEngineer = new RobotEngineer(oldStyleRobot);

robotEngineer.makeRobot();
const aRobot: Robot = robotEngineer.getRobot();

console.log("robot head :", aRobot.head);
console.log("robot torso:", aRobot.torso);
console.log("robot arms :", aRobot.arms);
console.log("robot legs :", aRobot.legs);
