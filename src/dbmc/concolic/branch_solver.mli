
val check_solver : Branch.t -> Formula_tracker.t -> Branch_tracker.t -> [ `Unsolvable of Branch_tracker.Status.t | `Solved of Z3.Model.model ]