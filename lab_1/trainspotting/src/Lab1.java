import TSim.*;
import TSim.TSensor.sType;

import java.util.ArrayList;
import java.util.Deque;

public class Lab1 {
	private ArrayList<TZone> zones = new ArrayList<>();
	private TZone startZone1;
	private TZone startZone2;

	public Lab1(int speed1, int speed2) {
		TSimInterface tsi = TSimInterface.getInstance();
		this.zones = zoneConfig(tsi);

		try {
			tsi.setDebug(true);
			tsi.setSpeed(1, speed1);
			tsi.setSpeed(2, speed2);
			Thread th1 = new Thread(new TrainHandler(1, speed1, zones, tsi, startZone1));
			Thread th2 = new Thread(new TrainHandler(2, speed2, zones, tsi, startZone2));
			th1.start();
			th2.start();
		} catch (CommandException e) {
			e.printStackTrace(); // or only e.getMessage() for the error
			System.exit(1);
		}
	}

	public class TrainHandler implements Runnable {
		TSimInterface tsi;
		private int id;
		public int speed;
		private ArrayList<TZone> zones;
		private Deque<TZone> activeZones;
		private TSensor prevSensor;

		public TrainHandler(int id, int speed, ArrayList<TZone> zones, TSimInterface tsi, TZone startZone) {
			this.tsi = tsi;
			this.id = id;
			this.speed = speed;
			this.zones = zones;
			this.activeZones = new java.util.LinkedList<>();
			activeZones.addFirst(startZone);
		}

		@Override
		public void run() {
			while (true) {
				try {
					SensorEvent sEvent = tsi.getSensor(id);
					for (TZone zone : zones) {
						TSensor sensor;
						if (zone.hasSensor(sEvent)) {
							sensor = zone.getSensor(sEvent);
							handleEvent(sEvent, zone, sensor);
						}
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}

		private void handleEvent(SensorEvent event, TZone zone, TSensor sensor)
				throws InterruptedException, CommandException {

			if (event.getStatus() == SensorEvent.INACTIVE) {
				return;
			}

			System.out.println("EVENT:" + zone.getName());
			System.out.println("	actives:");
			for (TZone aZone : activeZones) {
				System.out.println("	-" + aZone.getName());
			}

			if (stationCase(event, sensor)) {
				reverse();
				System.out.println("Train " + id + " is at station " + zone.getName());
			} else if (leavingZone(zone) && sensor.getType() != sType.ACQUIRE && prevSensor != null
					&& prevSensor.getType() != sType.ACQUIRE) {
				releaseZone(zone);
				System.out.println("Train " + id + " is leaving " + zone.getName());
			} else if (stopCase(event, zone, sensor) && sensor.getType() != sType.RELEASE && prevSensor != null
					&& prevSensor.getType() != sType.RELEASE) {
				stop();
				System.out.println("Train " + id + " is stopping before " + zone.getName());
				acquireZone(zone);
				System.out.println("Adjusting switch: " + "prevzone:" + activeZones.peekLast().getName() + " nextzone:"
						+ zone.getName());
				sensor.adjustSwitch(activeZones.getLast(), zone);
				accelerate();
				System.out.println("Train " + id + " is entering " + zone.getName());
			} else if (enteringZone(zone) && sensor.getType() != sType.RELEASE && prevSensor != null
					&& prevSensor.getType() != sType.RELEASE) {
				acquireZone(zone);
				System.out.println("Train " + id + " is entering " + zone.getName());
				sensor.adjustSwitch(activeZones.getLast(), zone);
			}

			prevSensor = sensor;
		}

		private void acquireZone(TZone zone) throws InterruptedException {
			zone.getSemaphore().acquire();
			activeZones.addFirst(zone);
		}

		private void releaseZone(TZone zone) {
			zone.getSemaphore().release();
			activeZones.remove(zone);
			System.out.println("Removed" + zone.getName());
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

		private boolean stationCase(SensorEvent event, TSensor sensor) {
			return event.getStatus() == SensorEvent.ACTIVE && sensor.isStation();
		}

		private boolean stopCase(SensorEvent event, TZone zone, TSensor sensor) {
			return !sensor.hasFreeZone() && zone.isActive() && !activeZones.contains(zone);
		}

		private boolean leavingZone(TZone zone) {
			return zone.isActive() && activeZones.contains(zone);
		}

		private boolean enteringZone(TZone zone) {
			return !zone.isActive() && !activeZones.contains(zone);
		}
	}

	private ArrayList<TZone> zoneConfig(TSimInterface tsi) {
		// create sensors
		TSensor A1 = new TSensor(tsi, 15, 3, sType.STATION);
		TSensor B1 = new TSensor(tsi, 19, 7, sType.ACQUIRE);
		TSensor C1 = new TSensor(tsi, 15, 7, sType.RELEASE);

		TSensor A2 = new TSensor(tsi, 15, 5, sType.STATION);
		TSensor B2 = new TSensor(tsi, 19, 7, sType.ACQUIRE);
		TSensor C2 = new TSensor(tsi, 14, 8, sType.RELEASE);

		TSensor A3 = new TSensor(tsi, 6, 7);
		TSensor B3 = new TSensor(tsi, 10, 7);
		TSensor C3 = new TSensor(tsi, 8, 5);
		TSensor D3 = new TSensor(tsi, 10, 8);

		TSensor A4 = new TSensor(tsi, 14, 8);
		TSensor B4 = new TSensor(tsi, 15, 7);
		TSensor C4 = new TSensor(tsi, 13, 10);
		TSensor D4 = new TSensor(tsi, 13, 9);

		TSensor A5 = new TSensor(tsi, 17, 9, sType.ACQUIRE);
		TSensor B5 = new TSensor(tsi, 2, 9, sType.ACQUIRE);
		TSensor C5 = new TSensor(tsi, 6, 10, sType.RELEASE);
		TSensor D5 = new TSensor(tsi, 13, 10, sType.RELEASE);

		TSensor A6 = new TSensor(tsi, 17, 9, sType.ACQUIRE);
		TSensor B6 = new TSensor(tsi, 2, 9, sType.ACQUIRE);
		TSensor C6 = new TSensor(tsi, 6, 9, sType.RELEASE);
		TSensor D6 = new TSensor(tsi, 13, 9, sType.RELEASE);

		TSensor A7 = new TSensor(tsi, 6, 10);
		TSensor B7 = new TSensor(tsi, 6, 9);
		TSensor C7 = new TSensor(tsi, 6, 11);
		TSensor D7 = new TSensor(tsi, 6, 13);

		TSensor A8 = new TSensor(tsi, 15, 11, sType.STATION);
		TSensor B8 = new TSensor(tsi, 1, 11, sType.ACQUIRE);
		TSensor C8 = new TSensor(tsi, 6, 11, sType.RELEASE);

		TSensor A9 = new TSensor(tsi, 15, 13, sType.STATION);
		TSensor B9 = new TSensor(tsi, 1, 11, sType.ACQUIRE);
		TSensor C9 = new TSensor(tsi, 6, 13, sType.RELEASE);

		// create zones (stations with trains are start zones)
		TZone Z1 = new TZone(tsi, "ZONE 1", true);
		TZone Z2 = new TZone(tsi, "ZONE 2");
		TZone Z3 = new TZone(tsi, "ZONE 3");
		TZone Z4 = new TZone(tsi, "ZONE 4");
		TZone Z5 = new TZone(tsi, "ZONE 5");
		TZone Z6 = new TZone(tsi, "ZONE 6");
		TZone Z7 = new TZone(tsi, "ZONE 7");
		TZone Z8 = new TZone(tsi, "ZONE 8", true);
		TZone Z9 = new TZone(tsi, "ZONE 9");

		Z1.addSensor(A1);
		Z1.addSensor(B1);
		Z1.addSensor(C1);

		Z2.addSensor(A2);
		Z2.addSensor(B2);
		Z2.addSensor(C2);

		Z3.addSensor(A3);
		Z3.addSensor(B3);
		Z3.addSensor(C3);
		Z3.addSensor(D3);

		Z4.addSensor(A4);
		Z4.addSensor(B4);
		Z4.addSensor(C4);
		Z4.addSensor(D4);

		Z5.addSensor(A5);
		Z5.addSensor(B5);
		Z5.addSensor(C5);
		Z5.addSensor(D5);

		Z6.addSensor(A6);
		Z6.addSensor(B6);
		Z6.addSensor(C6);
		Z6.addSensor(D6);

		Z7.addSensor(A7);
		Z7.addSensor(B7);
		Z7.addSensor(C7);
		Z7.addSensor(D7);

		Z8.addSensor(A8);
		Z8.addSensor(B8);
		Z8.addSensor(C8);

		Z9.addSensor(A9);
		Z9.addSensor(B9);
		Z9.addSensor(C9);

		// add adjacent zones to sensors
		A1.addAdjZone(Z1);
		A1.addAdjZone(Z2);
		B1.addAdjZone(Z1);
		B1.addAdjZone(Z2);

		A5.addAdjZone(Z6);
		A5.addAdjZone(Z5);
		A6.addAdjZone(Z6);
		A6.addAdjZone(Z5);

		B5.addAdjZone(Z5);
		B5.addAdjZone(Z6);
		B6.addAdjZone(Z5);
		B6.addAdjZone(Z6);

		B8.addAdjZone(Z8);
		B8.addAdjZone(Z9);
		B9.addAdjZone(Z8);
		B9.addAdjZone(Z9);

		// create switches
		TSwitch S1 = new TSwitch(tsi, 17, 7);
		TSwitch S2 = new TSwitch(tsi, 15, 9);
		TSwitch S3 = new TSwitch(tsi, 4, 9);
		TSwitch S4 = new TSwitch(tsi, 3, 11);

		// add switches to sensors that will trigger them
		B1.addSwitch(S1);
		B2.addSwitch(S1);
		A4.addSwitch(S1);
		B4.addSwitch(S1);

		A5.addSwitch(S2);
		A6.addSwitch(S2);
		C4.addSwitch(S2);
		D4.addSwitch(S2);

		B5.addSwitch(S3);
		B6.addSwitch(S3);
		A7.addSwitch(S3);
		B7.addSwitch(S3);

		B8.addSwitch(S4);
		B9.addSwitch(S4);
		C7.addSwitch(S4);
		D7.addSwitch(S4);

		// add zone links for each "direction" of the switches
		S1.addZone(Z1, TSwitch.Dir.LEFT);
		S1.addZone(Z4, TSwitch.Dir.LEFT);
		S1.addZone(Z2, TSwitch.Dir.RIGHT);
		S1.addZone(Z4, TSwitch.Dir.RIGHT);

		S2.addZone(Z4, TSwitch.Dir.LEFT);
		S2.addZone(Z6, TSwitch.Dir.LEFT);
		S2.addZone(Z4, TSwitch.Dir.RIGHT);
		S2.addZone(Z5, TSwitch.Dir.RIGHT);

		S3.addZone(Z6, TSwitch.Dir.RIGHT);
		S3.addZone(Z7, TSwitch.Dir.RIGHT);
		S3.addZone(Z5, TSwitch.Dir.LEFT);
		S3.addZone(Z7, TSwitch.Dir.LEFT);

		S4.addZone(Z8, TSwitch.Dir.RIGHT);
		S4.addZone(Z7, TSwitch.Dir.RIGHT);
		S4.addZone(Z7, TSwitch.Dir.LEFT);
		S4.addZone(Z9, TSwitch.Dir.LEFT);

		// add zones to list
		ArrayList<TZone> result = new ArrayList<>();
		result.add(Z1);
		result.add(Z2);
		result.add(Z3);
		result.add(Z4);
		result.add(Z5);
		result.add(Z6);
		result.add(Z7);
		result.add(Z8);
		result.add(Z9);

		// add start zones for trains
		startZone1 = Z1;
		startZone2 = Z8;

		return result;
	}
}
