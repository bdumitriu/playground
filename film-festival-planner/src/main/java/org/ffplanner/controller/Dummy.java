package org.ffplanner.controller;

import javax.enterprise.context.ConversationScoped;
import javax.inject.Named;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
@Named
@ConversationScoped
public class Dummy implements Serializable {

    private static final long serialVersionUID = 1L;

    private final Collection<Integer> hours;

    private Collection<AgendaEntry> agendaEntries;

    private Map<Integer, String> colors;

    public Dummy() {
        hours = new LinkedList<>();
        for (Integer i = 7; i <= 20; i++) {
            hours.add(i);
        }
        colors = new HashMap<>();
    }

    public Collection<Integer> getHours() {
        return hours;
    }

    public void createEntries() {
        System.out.println("createEntries");
        agendaEntries = new LinkedList<>();
        agendaEntries.add(new AgendaEntry(8, "dentist"));
        agendaEntries.add(new AgendaEntry(10, "interview"));
        agendaEntries.add(new AgendaEntry(15, "lunch"));
        colors.clear();
        for (AgendaEntry agendaEntry : agendaEntries) {
            colors.put(agendaEntry.hour, "red");
        }
    }

    public boolean hasEntryForHour(Integer hour) {
        System.out.println("hasEntryForHour(" + hour + ")");
        return getEntryForHour(hour) != null;
    }

    public AgendaEntry getEntryForHour(Integer hour) {
        System.out.println("getEntryForHour(" + hour + ")");
        for (AgendaEntry agendaEntry : agendaEntries) {
            if (agendaEntry.hour == hour) {
                return agendaEntry;
            }
        }
        return null;
    }

    public String getColor(int hour) {
        return colors.get(hour);
    }

    public void changeColor(int hour) {
        System.out.println("change color (" + hour + ")");
        colors.put(hour, getToggled(colors.get(hour)));
    }

    private static String getToggled(String color) {
        return color.equals("red") ? "blue" : "red";
    }

    public static class AgendaEntry {

        private final int hour;

        private final String event;

        private AgendaEntry(int hour, String event) {
            this.hour = hour;
            this.event = event;
        }

        public int getHour() {
            return hour;
        }

        public String getEvent() {
            return event;
        }
    }
}
