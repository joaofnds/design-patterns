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
  public add(newSongComponent: SongComponent) {
    throw new Error("Method not implemented.");
  }

  public remove(songComponent: SongComponent) {
    throw new Error("Method not implemented.");
  }

  public getComponent(componentIndex: number): SongComponent {
    throw new Error("Method not implemented.");
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

  public displaySongInfo(): void {
    throw new Error("Method not implemented.");
  }
}

class SongGroup extends SongComponent {
  private songComponents: Array<SongComponent> = new Array();

  private groupName: string;
  private groupDescription: string;

  public constructor(newGroupName: string, newGroupDescription: string) {
    super();
    this.groupName = newGroupName;
    this.groupDescription = newGroupDescription;
  }

  public getGroupName(): string {
    return this.groupName;
  }

  public getGroupDescription(): string {
    return this.groupDescription;
  }

  public add(newSongComponent: SongComponent): void {
    this.songComponents.push(newSongComponent);
  }

  public remove(songComponent: SongComponent): void {
    const indexOfSong = this.songComponents.indexOf(songComponent);
    if (indexOfSong !== -1) {
      this.songComponents.splice(indexOfSong, 1);
    }
  }

  public getComponent(componentIndex: number): SongComponent {
    return this.songComponents[componentIndex];
  }

  public displaySongInfo(): void {
    console.log(this.getGroupName(), this.getGroupDescription());
    for (const song of this.songComponents) {
      (song as Song).displaySongInfo();
    }
  }
}

class Song extends SongComponent {
  private songName: string;
  private bandName: string;
  private releaseYear: number;

  public constructor(
    newSongName: string,
    newBandName: string,
    newReleaseYear: number
  ) {
    super();
    this.songName = newSongName;
    this.bandName = newBandName;
    this.releaseYear = newReleaseYear;
  }

  public getSongName(): string {
    return this.songName;
  }

  public getBandName(): string {
    return this.bandName;
  }

  public getReleaseYear(): number {
    return this.releaseYear;
  }

  public displaySongInfo(): void {
    console.log(
      `${this.getSongName()} was released by ${this.getBandName()} in ${this.getReleaseYear()}`
    );
  }
}

class DiscJockey {
  private songList: SongComponent;

  public constructor(newSongList: SongComponent) {
    this.songList = newSongList;
  }

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
