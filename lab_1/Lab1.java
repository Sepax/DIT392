import TSim.*;

import java.util.concurrent.Semaphore;

public class Lab1 {

  public Lab1(int speed1, int speed2) throws InterruptedException {
    TSimInterface tsi = TSimInterface.getInstance();

    try {
      tsi.setSpeed(1, speed1);
      tsi.setSpeed(2, speed2);
      Thread train1 = new Thread(new Train(1, Dir.RIGHT, speed1, tsi));
      Thread train2 = new Thread(new Train(2, Dir.LEFT, speed2, tsi));
      train1.start();
      train2.start();
      // SensorEvent s1 = tsi.getSensor(1);
      // System.out.println("Manual SensorEvent:" + s1);
    }

    catch (CommandException e) {
      System.exit(1);
      e.printStackTrace(); // or only e.getMessage() for the error

    }
  }

  public enum Dir {
    LEFT, RIGHT;
  }

  public class Train implements Runnable {
    TSimInterface tsi;
    private int id;
    private Dir dir;
    private int initialSpeed;
    private static Semaphore criticalSection0 = new Semaphore(1);
    private static Semaphore criticalSection1 = new Semaphore(1);
    private static Semaphore criticalSection2 = new Semaphore(1);
    private static Semaphore criticalSection3 = new Semaphore(1);
    private static Semaphore criticalSection4 = new Semaphore(1);
    private static Semaphore criticalSection5 = new Semaphore(1);

    public Train(int id, Dir dir, int speed, TSimInterface tsi) {
      this.tsi = tsi;
      this.id = id;
      this.dir = dir;
      this.initialSpeed = speed;
    }

    public void flipTrain() throws CommandException { // TODO, fix exception l8r
      // Implement changing trains actual direction
      if (dir == Dir.LEFT) {
        dir = Dir.RIGHT;
        stopTrain();
        try {
          Thread.sleep(1000);
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        }
        initialSpeed = -initialSpeed;
        tsi.setSpeed(id, initialSpeed);
      } else {
        dir = Dir.LEFT;
        stopTrain();
        try {
          Thread.sleep(1000);
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        }
        initialSpeed = -initialSpeed;
        tsi.setSpeed(id, initialSpeed);
      }
    }

    public void stopTrain() throws CommandException { // TODO, fix exception l8r
      System.out.println("Stopping train number " + id + "!");
      tsi.setSpeed(id, 0);

    }

    public void startTrain() throws CommandException { // TODO, fix exception l8r
      tsi.setSpeed(id, initialSpeed);
    }

    public void switchASwitch(int xPos, int yPos, int dir) throws CommandException { // TODO, fix exception l8r

      tsi.setSpeed(id, initialSpeed);
      // System.out.println("Before trying to switch");
      tsi.setSwitch(xPos, yPos, dir);
      // System.out.println("After setting switch");
    }

    // Returns if we where able to enter the cricital section
    public boolean acquireOrRelease(Semaphore sem, Dir acqDir, Dir tDir) throws InterruptedException { // TODO, fix
                                                                                                       // exception l8r
      if (tDir == acqDir) {
        System.out.println("Train " + id + " is approaching " + sem);
        System.out.println("Q len: " + sem.getQueueLength());
        sem.acquire();
      } else {
        sem.release();
      }
      return !(sem.getQueueLength() > 0);
    }

    @Override
    public void run() {
      try {
        while (true) {
          SensorEvent sEvent = tsi.getSensor(id);

          if (sEvent.getStatus() == 1) {
            int sE_x = sEvent.getXpos();
            int sE_y = sEvent.getYpos();
            String xy = sE_x + "," + sE_y;
            switch (xy) {

              case "15,3":
                System.out.println("Train direction is: " + dir);
                if (dir == Dir.LEFT) {
                  flipTrain();
                }
                break;

              case "15,5":
                System.out.println("Train direction is: " + dir);
                if (dir == Dir.LEFT) {
                  flipTrain();
                }
                break;

              case "15,11":
                System.out.println("Train direction is: " + dir);
                if (dir == Dir.RIGHT) {
                  flipTrain();
                }
                break;

              case "15,13":
                System.out.println("Train direction is: " + dir);
                if (dir == Dir.RIGHT) {
                  flipTrain();
                }
                break;

              case "6,7":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (6,7) from right");

                  if (criticalSection0.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection0.acquire();
                  startTrain();
                } else {
                  System.out.println("Train " + id + " on (6,7) from left");

                  criticalSection0.release();
                }
                break;

              case "8,5":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (8,5) from right");

                  if (criticalSection0.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection0.acquire();
                  startTrain();
                } else {
                  System.out.println("Train " + id + " on (8,5) from left");
                  criticalSection0.release();
                }
                break;

              case "10,7":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (10,7) from right");

                  criticalSection0.release();
                } else {
                  System.out.println("Train " + id + " on (10,7) from left");

                  if (criticalSection0.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection0.acquire();
                  startTrain();
                }
                break;

              case "9,8":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (9,8) from right");

                  criticalSection0.release();
                } else {
                  System.out.println("Train " + id + " on (9,8) from left");

                  if (criticalSection0.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection0.acquire();
                  startTrain();
                }
                break;

              case "15,7":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (15,7) from right");

                  if (criticalSection1.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection1.acquire();
                  startTrain();
                  switchASwitch(17, 7, 2);
                } else {
                  System.out.println("Train " + id + " on (15,7) from left");
                  criticalSection1.release();
                }
                break;

              case "19,7":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (19,7) from right");

                  if (criticalSection5.availablePermits() == 0) { // This tells us train came from CS5
                    criticalSection5.release();
                  }
                } else {
                  System.out.println("Train " + id + " on (19,7) from left");

                  if (criticalSection5.availablePermits() == 0) {
                    switchASwitch(17, 7, 2);
                  } else {
                    criticalSection5.acquire();
                    switchASwitch(17, 7, 1);
                  }
                }
                break;

              case "16,8":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (16,8) from right");

                  if (criticalSection1.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection1.acquire();
                  startTrain();
                  switchASwitch(17, 7, 1);
                } else {
                  System.out.println("Train " + id + " on (16,8) from left");
                  criticalSection1.release();
                }
                break;

              case "17,9":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (17,9) from right");

                  if (criticalSection2.availablePermits() == 0) {
                    switchASwitch(15, 9, 1);
                  } else {
                    criticalSection2.acquire();
                    switchASwitch(15, 9, 2);
                  }
                } else {
                  System.out.println("Train " + id + " on (17,9) from left");

                  if (criticalSection2.availablePermits() == 0) { // This tells us train came from CS2
                    criticalSection2.release();
                  }
                }
                break;

              case "13,9":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (13,9) from right");

                  criticalSection1.release();
                } else {
                  System.out.println("Train " + id + " on (13,9) from left");

                  if (criticalSection1.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection1.acquire();
                  startTrain();
                  switchASwitch(15, 9, 2);
                }
                break;

              case "14,10":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (14,10) from right");

                  criticalSection1.release();
                } else {
                  System.out.println("Train " + id + " on (14,10) from left");

                  if (criticalSection1.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection1.acquire();
                  startTrain();
                  switchASwitch(15, 9, 1);
                }
                break;

              case "6,9":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (6,9) from right");

                  if (criticalSection3.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection3.acquire();
                  startTrain();
                  switchASwitch(4, 9, 1);
                } else {
                  System.out.println("Train " + id + " on (6,9) from left");

                  criticalSection3.release();
                }
                break;

              case "5,10":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (5,10) from right");

                  if (criticalSection3.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection3.acquire();
                  startTrain();
                  switchASwitch(4, 9, 2);
                } else {
                  System.out.println("Train " + id + " on (5,10) from left");

                  criticalSection3.release();
                }
                break;

              case "2,9":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (2,9) from right");

                  if (criticalSection2.availablePermits() == 0) { // This tells us train came from CS2
                    criticalSection2.release();
                  }
                } else {
                  System.out.println("Train " + id + " on (2,9) from left");

                  if (criticalSection2.availablePermits() == 0) {
                    switchASwitch(4, 9, 2);
                  } else {
                    criticalSection2.acquire();
                    switchASwitch(4, 9, 1);
                  }
                }
                break;

              case "1,11":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (1,11) from right");

                  if (criticalSection4.availablePermits() == 0) {
                    switchASwitch(3, 11, 1);
                  } else {
                    criticalSection4.acquire();
                    switchASwitch(3, 11, 2);
                  }
                } else {
                  System.out.println("Train " + id + " on (1,11) from left");

                  if (criticalSection4.availablePermits() == 0) { // This tells us train came from CS4
                    criticalSection4.release();
                  }
                }
                break;

              case "3,13":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (3,13) from right");

                  criticalSection3.release();
                } else {
                  System.out.println("Train " + id + " on (3,13) from left");

                  if (criticalSection3.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection3.acquire();
                  startTrain();
                  switchASwitch(3, 11, 2);
                }
                break;

              case "5,11":
                if (dir == Dir.RIGHT) {
                  System.out.println("Train " + id + " on (5,11) from right");

                  criticalSection3.release();
                } else {
                  System.out.println("Train " + id + " on (5,11) from left");

                  if (criticalSection3.availablePermits() == 0) {
                    stopTrain();
                  }
                  criticalSection3.acquire();
                  startTrain();
                  switchASwitch(3, 11, 1);
                }
                break;

              default:
                System.out.println("IT SHOULD BE AN EXCEPTION HERE"); // TODO, throw exception.
            }
          }

        }
      } catch (InterruptedException e) {
        e.printStackTrace();
      } catch (CommandException e) {
        // System.exit(1);
        e.printStackTrace();
      }
    }
  }
}
