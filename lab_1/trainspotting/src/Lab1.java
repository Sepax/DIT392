import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.concurrent.Semaphore;

import TSim.*;

public class Lab1 {

	public Lab1(int speed1, int speed2) {
		TSimInterface tsi = TSimInterface.getInstance();
		ArrayList<Semaphore> semaphores;

		try {
			semaphores = createSemaphores(9);
			tsi.setSpeed(1, speed1);
			tsi.setSpeed(2, speed2);

			Thread th1 = new Thread(new TrainHandler(tsi, semaphores, 1, speed1, TrainHandler.Destination.SOUTH));
			Thread th2 = new Thread(new TrainHandler(tsi, semaphores, 2, speed2, TrainHandler.Destination.NORTH));

			th1.start();
			th2.start();

		} catch (CommandException e) {
			e.printStackTrace(); // or only e.getMessage() for the error
			System.exit(1);
		}
	}

	private ArrayList<Semaphore> createSemaphores(int amount) {
		ArrayList<Semaphore> semaphores = new ArrayList<Semaphore>();

		for (int i = 0; i < amount; i++) {
			semaphores.add(new Semaphore(1));
		}

		return semaphores;
	}

	public class TrainHandler implements Runnable {
		private TSimInterface tsi;
		private int id;
		private int speed;
		private Destination dest;

		private ArrayList<SensorEvent> events;
		private ArrayList<TSensor> sensors;
		private ArrayList<TZone> zones;
		private ArrayList<TZone> activeZones;

		public static final String ANSI_RESET = "\u001B[0m";
		public static final String ANSI_BLACK = "\u001B[30m";
		public static final String ANSI_RED = "\u001B[31m";
		public static final String ANSI_GREEN = "\u001B[32m";
		public static final String ANSI_YELLOW = "\u001B[33m";
		public static final String ANSI_BLUE = "\u001B[34m";
		public static final String ANSI_PURPLE = "\u001B[35m";
		public static final String ANSI_CYAN = "\u001B[36m";
		public static final String ANSI_WHITE = "\u001B[37m";

		public static final String ANSI_BLACK_BACKGROUND = "\u001B[40m";
		public static final String ANSI_RED_BACKGROUND = "\u001B[41m";
		public static final String ANSI_GREEN_BACKGROUND = "\u001B[42m";
		public static final String ANSI_YELLOW_BACKGROUND = "\u001B[43m";
		public static final String ANSI_BLUE_BACKGROUND = "\u001B[44m";
		public static final String ANSI_PURPLE_BACKGROUND = "\u001B[45m";
		public static final String ANSI_CYAN_BACKGROUND = "\u001B[46m";
		public static final String ANSI_WHITE_BACKGROUND = "\u001B[47m";

		private enum Destination {
			NORTH, SOUTH
		}

		public TrainHandler(TSimInterface tsi, ArrayList<Semaphore> semaphores, int id, int speed, Destination dest) {
			this.tsi = tsi;
			this.id = id;
			this.speed = speed;
			this.dest = dest;
			this.sensors = new ArrayList<TSensor>();
			this.zones = new ArrayList<TZone>();
			this.activeZones = new ArrayList<TZone>();

			initConfig(semaphores);
		}

		@Override
		public void run() {
			while (true) {
				try {
					SensorEvent sEvent = tsi.getSensor(id);
					for (TSensor sensor : sensors) {
						if (sensor.isActive(sEvent)) {

							handleEvent(sensor, sEvent);
						}
					}

				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}

		public void handleEvent(TSensor sensor, SensorEvent sEvent) throws InterruptedException, CommandException {
			// If sensor is a station, toggle destination and reverse the train
			if (sensor.isStation()) {
				toggleDestination();
				reverse();
				return;
			}

			for (TZone zone : sensor.getZones()) {
				// If the train is in the zone, release the semaphore for the zone (train is
				// leaving zone)
				if (activeZones.contains(zone)) {
					zone.getSemaphore().release();
					activeZones.remove(zone);
					System.out
							.println(ANSI_GREEN_BACKGROUND + "Train " + id + " left zone " + zone.getId() + ANSI_RESET);
					return;
				}
			}

			// Go through all zones that the sensor can trigger
			for (TZone zone : sensor.getZones()) {

				// If the train is not in the zone, try to acquire the semaphore for the zone
				// (train is entering zone)
				if (zone.getSemaphore().tryAcquire()) {
					activeZones.add(zone);
					adjustSwitch(sensor, zone);
					System.out.println(
							ANSI_GREEN_BACKGROUND + "Train " + id + " entered zone " + zone.getId() + ANSI_RESET);
					return;
				}
			}

			// If no zone was aquired, stop the train and wait for the first zone to be
			// released.
			TZone nextZone = sensor.getZones().get(0);
			stop();
			System.out
					.println(ANSI_RED_BACKGROUND + "Train " + id + " stopped before " + nextZone.getId() + ANSI_RESET);
			nextZone.getSemaphore().acquire();
			System.out
					.println(ANSI_GREEN_BACKGROUND + "Train " + id + " entered zone " + nextZone.getId() + ANSI_RESET);
			activeZones.add(nextZone);
			adjustSwitch(sensor, nextZone);
			accelerate();
		}

		private void accelerate() {
			try {
				tsi.setSpeed(id, speed);
			} catch (CommandException e) {
				e.printStackTrace();
			}
		}

		private void stop() {
			try {
				tsi.setSpeed(id, 0);
			} catch (CommandException e) {
				e.printStackTrace();
			}
		}

		private void sleep() {
			try {
				Thread.sleep((1000 + (20 * Math.abs(speed))));
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}

		private void reverse() {
			speed = -speed;
			stop();
			sleep();
			accelerate();
		}

		private void toggleDestination() {
			if (dest == Destination.NORTH) {
				dest = Destination.SOUTH;
			} else {
				dest = Destination.NORTH;
			}
		}

		private void adjustSwitch(TSensor sensor, TZone zone) throws CommandException {
			if (!sensor.hasSwitch()) {
				return;
			}

			sensor.getSwitch().adjust(tsi, activeZones);
			System.out.println(ANSI_YELLOW_BACKGROUND + "Train " + id + " adjusted switch " + sensor.getSwitch().getId()
					+ ANSI_RESET);
		}

		private void initConfig(ArrayList<Semaphore> semaphores) {
			for (Semaphore sem : semaphores) {
				zones.add(new TZone(zones.size() + 1, sem));
			}

			TZone Z1 = zones.get(0);
			TZone Z2 = zones.get(1);
			TZone Z3 = zones.get(2);
			TZone Z4 = zones.get(3);
			TZone Z5 = zones.get(4);
			TZone Z6 = zones.get(5);
			TZone Z7 = zones.get(6);
			TZone Z8 = zones.get(7);
			TZone Z9 = zones.get(8);

			TSensor S1 = new TSensor(1, 15, 3, true);
			TSensor S2 = new TSensor(2, 14, 7);
			TSensor S3 = new TSensor(3, 15, 5, true);
			TSensor S4 = new TSensor(4, 14, 8);
			TSensor S5 = new TSensor(5, 6, 6);
			TSensor S6 = new TSensor(6, 10, 5);
			TSensor S7 = new TSensor(7, 10, 7);
			TSensor S8 = new TSensor(8, 10, 8);
			TSensor S9 = new TSensor(9, 9, 7);
			TSensor S10 = new TSensor(10, 17, 9);
			TSensor S11 = new TSensor(11, 13, 9);
			TSensor S12 = new TSensor(12, 13, 10);
			TSensor S13 = new TSensor(13, 6, 9);
			TSensor S14 = new TSensor(14, 6, 10);
			TSensor S15 = new TSensor(15, 2, 9);
			TSensor S16 = new TSensor(16, 1, 11);
			TSensor S17 = new TSensor(17, 5, 11);
			TSensor S18 = new TSensor(18, 15, 11, true);
			TSensor S19 = new TSensor(19, 5, 13);
			TSensor S20 = new TSensor(20, 15, 13, true);

			S1.addZone(Z1);
			S9.addZone(Z1);

			S3.addZone(Z2);
			S9.addZone(Z2);

			S5.addZone(Z3);
			S6.addZone(Z3);
			S7.addZone(Z3);
			S8.addZone(Z3);

			S2.addZone(Z4);
			S4.addZone(Z4);
			S11.addZone(Z4);
			S12.addZone(Z4);

			S10.addZone(Z5);
			S10.addZone(Z6);
			S15.addZone(Z6);
			S15.addZone(Z5);

			S13.addZone(Z7);
			S14.addZone(Z7);
			S17.addZone(Z7);
			S19.addZone(Z7);

			S16.addZone(Z8);
			S18.addZone(Z8);

			S16.addZone(Z9);
			S20.addZone(Z9);

			TSwitch SW1 = new TSwitch(1, 17, 7, Z1, Z2);
			TSwitch SW2 = new TSwitch(2, 15, 9, Z5, Z6);
			TSwitch SW3 = new TSwitch(3, 4, 9, Z6, Z5);
			TSwitch SW4 = new TSwitch(4, 3, 11, Z9, Z8);

			S2.setSwitch(SW1);
			S4.setSwitch(SW1);
			S9.setSwitch(SW1);

			S10.setSwitch(SW2);
			S11.setSwitch(SW2);
			S12.setSwitch(SW2);

			S13.setSwitch(SW3);
			S14.setSwitch(SW3);
			S15.setSwitch(SW3);

			S16.setSwitch(SW4);
			S17.setSwitch(SW4);
			S19.setSwitch(SW4);

			sensors.add(S1);
			sensors.add(S2);
			sensors.add(S3);
			sensors.add(S4);
			sensors.add(S5);
			sensors.add(S6);
			sensors.add(S7);
			sensors.add(S8);
			sensors.add(S9);
			sensors.add(S10);
			sensors.add(S11);
			sensors.add(S12);
			sensors.add(S13);
			sensors.add(S14);
			sensors.add(S15);
			sensors.add(S16);
			sensors.add(S17);
			sensors.add(S18);
			sensors.add(S19);
			sensors.add(S20);

			if (id == 1) {
				try {
					Z1.getSemaphore().acquire();
				} catch (Exception e) {
					e.printStackTrace();
				}
				activeZones.add(Z1);
			} else {
				try {
					Z8.getSemaphore().acquire();
				} catch (Exception e) {
					e.printStackTrace();
				}
				activeZones.add(Z8);
			}
		}
	}

	public class TZone {
		private int id;
		private Semaphore sem;

		public TZone(int id, Semaphore sem) {
			this.id = id;
			this.sem = sem;
		}

		public int getId() {
			return id;
		}

		public Semaphore getSemaphore() {
			return sem;
		}
	}

	public class TSensor {
		private int id;
		private int x;
		private int y;
		private boolean station;
		private ArrayList<TZone> zones;
		private TSwitch tSwitch;

		public TSensor(int id, int x, int y, boolean station) {
			this.id = id;
			this.x = x;
			this.y = y;
			this.station = station;
			this.zones = new ArrayList<TZone>();
		}

		public TSensor(int id, int x, int y) {
			this(id, x, y, false);
		}

		public int getId() {
			return id;
		}

		public int getX() {
			return x;
		}

		public int getY() {
			return y;
		}

		public ArrayList<TZone> getZones() {
			return zones;
		}

		public TSwitch getSwitch() {
			return tSwitch;
		}

		public boolean isStation() {
			return station;
		}

		public boolean hasSwitch() {
			return tSwitch != null;
		}

		public void addZone(TZone zone) {
			zones.add(zone);
		}

		public void setSwitch(TSwitch tSwitch) {
			this.tSwitch = tSwitch;
		}

		public boolean isActive(SensorEvent event) {
			return event.getStatus() == SensorEvent.ACTIVE && event.getXpos() == x && event.getYpos() == y;
		}
	}

	public class TSwitch {
		private int id;
		private int x;
		private int y;
		private TZone leftZone;
		private TZone rightZone;
		private int dir;

		public TSwitch(int id, int x, int y, TZone leftZone, TZone rightZone) {
			this.id = id;
			this.x = x;
			this.y = y;
			this.leftZone = leftZone;
			this.rightZone = rightZone;
		}

		public int getId() {
			return id;
		}

		public void adjust(TSimInterface tsi, ArrayList<TZone> activeZones) throws CommandException {
			if (activeZones.contains(leftZone)) {
				dir = 0;
			} else if (activeZones.contains(rightZone)) {
				dir = 1;
			}

			tsi.setSwitch(x, y, dir);
		}
	}
}
