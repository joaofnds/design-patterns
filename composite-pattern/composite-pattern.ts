/**
 * What is the Composite Design Pattern?
 * - Allows you to treat individual objects and
 *   compositions of objects uniformly
 * - They allow you to represent part-whole hierarchies
 * - Components can be further divided into smaller components
 * - You can structure data or represent the inner working of
 *   every part of a whole object individually
 *
 * @see http://www.newthinktank.com/2012/10/composite-design-pattern-tutorial/
 */

abstract class SongComponent {
  public abstract add(song: SongComponent): void;
  public abstract remove(song: SongComponent): void;
  public abstract getComponent(index: number): SongComponent;
  public abstract getSongName(): string;
  public abstract getBandName(): string;
  public abstract getReleaseYear(): number;
  public abstract displaySongInfo(): void;
}

class SongGroup extends SongComponent {
  private components: Array<SongComponent> = new Array();

  public constructor(
    private readonly name: string,
    private readonly description: string
  ) {
    super();
  }

  public getGroupName(): string {
    return this.name;
  }

  public getGroupDescription(): string {
    return this.description;
  }

  public add(song: SongComponent): void {
    this.components.push(song);
  }

  public remove(song: SongComponent): void {
    const songIndex = this.components.indexOf(song);
    if (songIndex !== -1) {
      this.components.splice(songIndex, 1);
    }
  }

  public getComponent(index: number): SongComponent {
    return this.components[index];
  }

  public displaySongInfo(): void {
    console.log(this.getGroupName(), this.getGroupDescription());

    for (const song of this.components) {
      song.displaySongInfo();
    }
  }

  public getSongName(): string {
    throw new Error("Method not implemented.");
  }

  public getBandName(): string {
    throw new Error("Method not implemented.");
  }

  public getReleaseYear(): number {
    throw new Error("Method not implemented.");
  }
}

class Song extends SongComponent {
  public constructor(
    private readonly name: string,
    private readonly band: string,
    private readonly releaseYear: number
  ) {
    super();
  }

  public getSongName(): string {
    return this.name;
  }

  public getBandName(): string {
    return this.band;
  }

  public getReleaseYear(): number {
    return this.releaseYear;
  }

  public displaySongInfo(): void {
    console.log(
      `${this.getSongName()} was released by ${this.getBandName()} in ${this.getReleaseYear()}`
    );
  }

  public add(_newSongComponent: SongComponent): void {
    throw new Error("Method not implemented.");
  }

  public remove(_songComponent: SongComponent): void {
    throw new Error("Method not implemented.");
  }

  public getComponent(_componentIndex: number): SongComponent {
    throw new Error("Method not implemented.");
  }
}

class DiscJockey {
  public constructor(private readonly songList: SongComponent) {}

  public getSongList(): void {
    this.songList.displaySongInfo();
  }
}

//------------------------------------------------------------------

const industrialMusic: SongComponent = new SongGroup(
  "Industrial",
  "is a style of experimental music that draws on transgressive and provocative themes"
);
const heavyMetalMusic: SongComponent = new SongGroup(
  "Heavy Metal",
  "is a genre of rock that developed in the late 1960s, largely in the UK and in the US"
);
const dubstepMusic: SongComponent = new SongGroup(
  "Dubstep",
  "is a genre of electronic dance music that originated in South London, England"
);

const everySong: SongComponent = new SongGroup(
  "Song List",
  "Every song available"
);

everySong.add(industrialMusic);
industrialMusic.add(new Song("Head Like a Hole", "NIN", 1990));
industrialMusic.add(new Song("Headhunter", "Front 242", 1988));

industrialMusic.add(dubstepMusic);
dubstepMusic.add(new Song("Centipede", "Knife Party", 2012));
dubstepMusic.add(new Song("Tetris", "Doctor P", 2011));

everySong.add(heavyMetalMusic);
heavyMetalMusic.add(new Song("War Pigs", "Black Sabath", 1970));
heavyMetalMusic.add(new Song("Ace of Spades", "Motorhead", 1980));

const crazyLarry: DiscJockey = new DiscJockey(everySong);
crazyLarry.getSongList();
