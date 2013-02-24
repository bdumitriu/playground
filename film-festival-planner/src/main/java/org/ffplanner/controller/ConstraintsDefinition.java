package org.ffplanner.controller;

import com.google.common.collect.LinkedListMultimap;
import com.google.common.collect.ListMultimap;
import org.ffplanner.def.ScheduleDefinition;
import org.ffplanner.entity.ScheduleConstraintType;

import java.util.List;
import java.util.Map;

/**
 * @author Bogdan Dumitriu
 */
public class ConstraintsDefinition implements ScheduleDefinition {

    private final ListMultimap<WatchType, Long> constraintIds = LinkedListMultimap.create();

    public void initializeFrom(Map<Long, ScheduleConstraintType> constraints) {
        constraintIds.clear();
        for (Map.Entry<Long, ScheduleConstraintType> entry : constraints.entrySet()) {
            constraintIds.put(entry.getValue().getWatchType(), entry.getKey());
        }
    }

    @Override
    public List<Long> getShowingIds(WatchType watchType) {
        return constraintIds.get(watchType);
    }
}
