pub mod exact_dyadic;
pub mod predicates;
pub mod implicit_point;
pub mod bounds2_index;
pub mod delaunay2;
pub mod planar_cdt;
pub mod planar_constraints;
pub mod planar_refinement;
pub mod contour_triangulation;
pub mod bounds3_index;
pub mod linear_piece_index;
pub mod proximity_index;
pub mod spatial_index;
pub mod surface_index;
pub mod triangle_intersection;
pub mod curve_intersection;
pub mod point_grid;
pub mod point_clusters;
pub mod point_snap;

pub mod deletion;
pub mod dissolve;
pub mod edge_collapse;
pub mod fuse_reduce;
#[doc(hidden)]
pub mod fuse_cleanup;
#[doc(hidden)]
pub mod deform;
pub mod fuse_rules;
#[cfg(test)]
#[allow(dead_code)]
mod test_support;
