import TSim.*;
import java.util.ArrayList;

public class Lab1 {
	private ArrayList<TZone> zones = new ArrayList<>();

	public Lab1(int speed1, int speed2) {
		TSimInterface tsi = TSimInterface.getInstance();
		this.zones = zoneConfig(tsi);

		try {
			tsi.setDebug(true);
			tsi.setSpeed(1, speed1);
			tsi.setSpeed(2, speed2);
			Thread th1 = new Thread(new TrainHandler(1, speed1, zones, tsi));
			Thread th2 = new Thread(new TrainHandler(2, speed2, zones, tsi));
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

		public TrainHandler(int id, int speed, ArrayList<TZone> zones, TSimInterface tsi) {
			this.tsi = tsi;
			this.id = id;
			this.speed = speed;
			this.zones = zones;
		}

		@Override
		public void run() {
			while (true) {
				try {
					SensorEvent sEvent = tsi.getSensor(id);
					for (TZone zone : zones) {
						zone.refresh(sEvent, speed);
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}

		public void setSpeed(int speed) {
			this.speed = speed;
		}

		public int getSpeed() {
			return speed;
		}
	}

	private ArrayList<TZone> zoneConfig(TSimInterface tsi) {
		// create sensors
		TSensor A1 = new TSensor(tsi, 15, 3, true);
		TSensor B1 = new TSensor(tsi, 7, 7);
		TSensor A2 = new TSensor(tsi, 15, 5, true);
		TSensor B2 = new TSensor(tsi, 16, 8);
		TSensor A3 = new TSensor(tsi, 9, 7);
		TSensor B3 = new TSensor(tsi, 15, 7);
		TSensor A4 = new TSensor(tsi, 18, 7);
		TSensor B4 = new TSensor(tsi, 16, 9);
		TSensor A5 = new TSensor(tsi, 14, 10);
		TSensor B5 = new TSensor(tsi, 5, 10);
		TSensor A6 = new TSensor(tsi, 14, 9);
		TSensor B6 = new TSensor(tsi, 5, 9);
		TSensor A7 = new TSensor(tsi, 3, 9);
		TSensor B7 = new TSensor(tsi, 2, 11);
		TSensor A8 = new TSensor(tsi, 6, 11);
		TSensor B8 = new TSensor(tsi, 15, 11, true);
		TSensor A9 = new TSensor(tsi, 4, 13);
		TSensor B9 = new TSensor(tsi, 15, 13, true);

		// create zones (stations are start zones)
		TZone Z1 = new TZone(tsi, "ZONE 1", A1, B1, true);
		TZone Z2 = new TZone(tsi, "ZONE 2", A2, B2);
		TZone Z3 = new TZone(tsi, "ZONE 3", A3, B3);
		TZone Z4 = new TZone(tsi, "ZONE 4", A4, B4);
		TZone Z5 = new TZone(tsi, "ZONE 5", A5, B5);
		TZone Z6 = new TZone(tsi, "ZONE 6", A6, B6);
		TZone Z7 = new TZone(tsi, "ZONE 7", A7, B7);
		TZone Z8 = new TZone(tsi, "ZONE 8", A8, B8, true);
		TZone Z9 = new TZone(tsi, "ZONE 9", A9, B9);

		// add adjacent zones to sensors
		A3.addAdjZone(Z1);
		B1.addAdjZone(Z2);
		A4.addAdjZone(Z2);
		A3.addAdjZone(Z2);
		B1.addAdjZone(Z3);
		A4.addAdjZone(Z3);
		B3.addAdjZone(Z4);
		B2.addAdjZone(Z4);
		A6.addAdjZone(Z4);
		A5.addAdjZone(Z4);
		B4.addAdjZone(Z5);
		A7.addAdjZone(Z5);
		B4.addAdjZone(Z6);
		A7.addAdjZone(Z6);
		B6.addAdjZone(Z7);
		B5.addAdjZone(Z7);
		A8.addAdjZone(Z7);
		A9.addAdjZone(Z7);
		B7.addAdjZone(Z8);
		B7.addAdjZone(Z9);

		// create switches
		TSwitch S1 = new TSwitch(17, 7);
		TSwitch S2 = new TSwitch(15, 9);
		TSwitch S3 = new TSwitch(4, 9);
		TSwitch S4 = new TSwitch(3, 11);

		// add zone links for each "direction" of the switches
		S1.addLeftZone(Z3);
		S1.addLeftZone(Z4);
		S1.addRightZone(Z2);
		S1.addRightZone(Z4);
		S2.addLeftZone(Z4);
		S2.addLeftZone(Z6);
		S2.addRightZone(Z4);
		S2.addRightZone(Z5);
		S3.addRightZone(Z6);
		S3.addRightZone(Z7);
		S3.addLeftZone(Z5);
		S3.addLeftZone(Z7);
		S4.addRightZone(Z7);
		S4.addRightZone(Z8);
		S4.addLeftZone(Z7);
		S4.addLeftZone(Z9);

		// add switches to sensors that will trigger them
		B3.addSwitch(S1);
		A4.addSwitch(S1);
		B2.addSwitch(S1);
		B4.addSwitch(S2);
		A6.addSwitch(S2);
		A5.addSwitch(S2);
		B6.addSwitch(S3);
		B5.addSwitch(S3);
		A7.addSwitch(S3);
		B7.addSwitch(S4);
		A8.addSwitch(S4);
		A9.addSwitch(S4);

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

		return result;
	}
}
