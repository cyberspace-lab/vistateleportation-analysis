# Data Summary

## Summative Data
Similar to trial data, but each value has also _mean or _median appended to the variable name. This indicates that the value is the mean or median of the corresponding variable across all trials for a participant. For example, `duration_searching_mean` would be the average duration of the searching phase across all trials for that participant.

## Trial data

| Variable | Description | Type | Origin |
|----------|-------------|------|--------|
| `pointingpoint_target_distance` | The straight-line (Euclidean) distance between the participant's pointing origin and the location of the target object. | Numeric | Calculated using `euclid_distance` from `pointingpoint_x`, `pointingpoint_y`, `object_x`, and `object_y`. |
| `pointingpoint_pointed_distance` | The straight-line (Euclidean) distance between the participant's pointing origin and the location where they actually pointed. | Numeric | Calculated using `euclid_distance` from `pointingpoint_x`, `pointingpoint_y`, `ConfirmedPointing_x`, and `ConfirmedPointing_y`. |
| `target_pointed_distance_difference` | The difference between the distance to where the participant pointed and the distance to the actual target, both measured from the pointing origin. A positive value means the pointed location was further from the origin than the target; a negative value means it was closer. | Numeric | Calculated as `pointingpoint_pointed_distance - pointingpoint_target_distance`. |
| `pointingpoint_target_angle` | The angle from the pointing origin to the target object's location, relative to a fixed reference direction (e.g., 0 degrees might be North or the positive x-axis). | Numeric (likely in radians or degrees) | Calculated using `angle_from_positions` from `pointingpoint_x`, `pointingpoint_y`, `object_x`, and `object_y`. |
| `pointingpoint_pointed_angle` | The angle from the pointing origin to the location where the participant actually pointed, relative to the same fixed reference direction. | Numeric (likely in radians or degrees) | Calculated using `angle_from_positions` from `pointingpoint_x`, `pointingpoint_y`, `ConfirmedPointing_x`, and `ConfirmedPointing_y`. |
| `pointed_angle_difference` | The difference between the angle of the pointed location and the angle of the target, both relative to the pointing origin. This represents the angular error in the pointing direction. | Numeric (likely in radians or degrees) | Calculated using `angle_diff` from `pointingpoint_pointed_angle` and `pointingpoint_target_angle`. |
| `time_experimentstarted` | Timestamp when the experiment officially began. | Numeric (timestamp) | Extracted from `df_events` based on the order of filtered events. |
| `times_item1`, `time_item2`, `time_item3`, `time_item4` | Timestamps when the participant visited specific items (Item 1 through Item 4). | Numeric (timestamps) | Extracted from `df_events` based on the order of filtered events. |
| `time_returnstarted` | Timestamp when the phase requiring the participant to return to a specific location began. | Numeric (timestamp) | Extracted from `df_events` based on the order of filtered events. |
| `time_returned` | Timestamp when the participant successfully returned to the designated location. | Numeric (timestamp) | Extracted from `df_events` based on the order of filtered events. |
| `time_pointingorigin_start`, `time_pointingorigin_end` | Start and end timestamps for a phase related to establishing or being at the pointing origin. | Numeric (timestamps) | Extracted from `df_events` based on the order of filtered events. |
| `time_pointing1_start`, `time_pointing1_end`, `time_pointing2_start`, `time_pointing2_end`, `time_pointing3_start`, `time_pointing3_end`, `time_pointing4_start`, `time_pointing4_end` | Start and end timestamps for individual pointing actions (Pointing 1 through Pointing 4). | Numeric (timestamps) | Extracted from `df_events` based on the order of filtered events. |
| `duration_searching` | The time elapsed during the initial searching phase. | Numeric (duration) | Calculated as `time_returnstarted - time_experimentstarted`. |
| `duration_return` | The time elapsed during the return phase. | Numeric (duration) | Calculated as `time_returned - time_returnstarted`. |
| `duration_pointing1`, `duration_pointing2`, `duration_pointing3`, `duration_pointing4` | The time elapsed for each individual pointing action. | Numeric (duration) | Calculated as `time_pointingX_end - time_pointingX_start`. |
| `distances` | A vector of the total distance traveled by the participant at the specific timestamps when phases started. | Numeric Vector (distances) | Extracted from `session$navr$data$distance_total` using the `timestamps`. |
| `distance_items` | The total distance traveled by the participant up to the start of the first phase (likely the item searching phase). | Numeric (distance) | The first element of the `distances` vector. |
| `distance_return` | The total distance traveled by the participant between the start of the first phase and the start of the second phase (likely the distance covered during the return phase). | Numeric (distance) | The second element of the `distances` vector. |
