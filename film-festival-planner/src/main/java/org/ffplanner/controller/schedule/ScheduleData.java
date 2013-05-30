package org.ffplanner.controller.schedule;

import org.ffplanner.bean.UserScheduleBean;
import org.ffplanner.bean.programme.FestivalEditionProgramme;
import org.ffplanner.entity.User;
import org.ffplanner.entity.UserSchedule;
import org.ffplanner.entity.UserScheduleShowing;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * @author Bogdan Dumitriu
 */
public class ScheduleData {

    private final UserScheduleBean userScheduleBean;

    private final FestivalEditionProgramme festivalProgramme;

    private final Set<Long> scheduledShowings;

    private UserSchedule userSchedule;

    public ScheduleData(UserScheduleBean userScheduleBean, FestivalEditionProgramme festivalProgramme) {
        this.userScheduleBean = userScheduleBean;
        this.festivalProgramme = festivalProgramme;
        this.scheduledShowings = new HashSet<>();
    }

    public int size() {
        return scheduledShowings.size();
    }

    public void loadFor(User user) {
        scheduledShowings.clear();
        userSchedule = userScheduleBean.findOrCreateBy(user.getId(), festivalProgramme.getFestivalEdition());
        addShowings();
    }

    private void addShowings() {
        final Set<UserScheduleShowing> showings = userSchedule.getShowings();
        for (UserScheduleShowing showing : showings) {
            scheduledShowings.add(showing.getShowingId());
        }
    }

    public boolean reloadNeeded() {
        final Date lastUpdate = userSchedule.getScheduleLastModified();
        final UserSchedule currentSchedule = userScheduleBean.find(userSchedule.getId());
        return scheduleDeleted(currentSchedule) || scheduleUpdatedSince(lastUpdate, currentSchedule);
    }

    private static boolean scheduleDeleted(UserSchedule currentSchedule) {
        return currentSchedule == null;
    }

    private static boolean scheduleUpdatedSince(Date lastUpdate, UserSchedule currentSchedule) {
        return currentSchedule != null
                && (lastUpdate == null || currentSchedule.getScheduleLastModified().after(lastUpdate));
    }

    public boolean isScheduled(Long showingId) {
        return scheduledShowings.contains(showingId);
    }
}
