
val check_solver : Branch.t -> Formula_tracker.t -> Branch_tracker.t -> [ `Unsolvable | `Timeout | `Solved of Z3.Model.model ]