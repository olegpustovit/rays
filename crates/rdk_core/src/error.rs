//! Rdk errors: the failing operation, a typed code and the message.

macro_rules! codes {
    ($($name:ident = $text:literal,)*) => {
        /// What went wrong. `as_str` is the stable code the Lisp diagnostics report.
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Code {
            $($name),*
        }

        impl Code {
            pub fn as_str(self) -> &'static str {
                match self {
                    $(Code::$name => $text),*
                }
            }

            fn parse(text: &str) -> Option<Code> {
                match text {
                    $($text => Some(Code::$name),)*
                    _ => None,
                }
            }
        }
    };
}

codes! {
    AmbiguousClassification = "ambiguous_classification",
    AttributeStorageMismatch = "attribute_storage_mismatch",
    Cancelled = "cancelled",
    DegenerateTriangle = "degenerate_triangle",
    ExactConstructionFailed = "exact_construction_failed",
    Fail = "fail",
    GeometryMismatch = "geometry_mismatch",
    Internal = "internal",
    InvalidArgument = "invalid_argument",
    InvalidAttribute = "invalid_attribute",
    InvalidAttributeMirror = "invalid_attribute_mirror",
    InvalidBlur = "invalid_blur",
    InvalidComplex = "invalid_complex",
    InvalidConnectivity = "invalid_connectivity",
    InvalidConstraints = "invalid_constraints",
    InvalidDirection = "invalid_direction",
    InvalidDistance = "invalid_distance",
    InvalidEdgeGroup = "invalid_edge_group",
    InvalidGeometry = "invalid_geometry",
    InvalidGroup = "invalid_group",
    InvalidInput = "invalid_input",
    InvalidInterpolate = "invalid_interpolate",
    InvalidName = "invalid_name",
    InvalidNormals = "invalid_normals",
    InvalidOffsets = "invalid_offsets",
    InvalidOutput = "invalid_output",
    InvalidParameter = "invalid_parameter",
    InvalidPattern = "invalid_pattern",
    InvalidPayload = "invalid_payload",
    InvalidPointIndex = "invalid_point_index",
    InvalidPolyCut = "invalid_poly_cut",
    InvalidPolygon = "invalid_polygon",
    InvalidPosition = "invalid_position",
    InvalidQuery = "invalid_query",
    InvalidRagged = "invalid_ragged",
    InvalidRay = "invalid_ray",
    InvalidRefinement = "invalid_refinement",
    InvalidRemap = "invalid_remap",
    InvalidRemesh = "invalid_remesh",
    InvalidSelection = "invalid_selection",
    InvalidSort = "invalid_sort",
    InvalidState = "invalid_state",
    InvalidSurface = "invalid_surface",
    InvalidTopology = "invalid_topology",
    InvalidTopologyEdgeLookup = "invalid_topology_edge_lookup",
    InvalidTopologyIndex = "invalid_topology_index",
    InvalidTransfer = "invalid_transfer",
    InvalidTransform = "invalid_transform",
    InvalidTriangulation = "invalid_triangulation",
    InvalidUv = "invalid_uv",
    MeshConversion = "mesh_conversion",
    MissingEdgeGroup = "missing_edge_group",
    MissingGroup = "missing_group",
    NonFiniteOutput = "non_finite_output",
    OpenOutput = "open_output",
    ParallelError = "parallel_error",
    PointGroupConflict = "point_group_conflict",
    PointPayloadConflict = "point_payload_conflict",
    PromotionCollision = "promotion_collision",
    RoundingCardinality = "rounding_cardinality",
    RoundingCollision = "rounding_collision",
    RoundingDegenerate = "rounding_degenerate",
    RoundingOrientation = "rounding_orientation",
    SchemaMismatch = "schema_mismatch",
    SeamSelfIntersection = "seam_self_intersection",
    SurfaceSelfIntersection = "surface_self_intersection",
    UnresolvedCleanup = "unresolved_cleanup",
    UnresolvedRoundingContacts = "unresolved_rounding_contacts",
    UnresolvedRoundingSlivers = "unresolved_rounding_slivers",
    UnsupportedStorage = "unsupported_storage",
    WorkerFailure = "worker_failure",
    Gpu = "E_GPU",
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct T {
    operation: String,
    code: Code,
    message: String,
}

pub fn new(operation: &str, code: Code, message: String) -> T {
    T { operation: operation.to_owned(), code, message }
}

/// A cancelled operation. Cancellation is the code, never a message to compare.
pub fn cancelled(operation: &str) -> T {
    new(operation, Code::Cancelled, "geometry operation was cancelled".to_owned())
}

/// `Error.guard`: a failure takes the guard's operation and code; a cancellation stays a cancellation.
pub fn guard(failure: T, operation: &str, code: Code) -> T {
    if is_cancelled(&failure) { cancelled(operation) } else { new(operation, code, failure.message) }
}

pub fn make(
    operation: String,
    code: String,
    message: String,
    _hints: Option<Vec<String>>,
) -> Result<T, T> {
    if operation.trim().is_empty() {
        return Err(new("Rdk.Error.make", Code::InvalidArgument, "Rdk.Error.make: empty operation".to_owned()));
    }
    if code.trim().is_empty() {
        return Err(new("Rdk.Error.make", Code::InvalidArgument, "Rdk.Error.make: empty code".to_owned()));
    }
    Ok(T {
        operation,
        code: Code::parse(&code).unwrap_or_else(|| panic!("unknown rdk error code {code}")),
        message,
    })
}

pub fn is_cancelled(value: &T) -> bool {
    value.code == Code::Cancelled
}

pub fn operation(value: &T) -> &str {
    &value.operation
}

pub fn kind(value: &T) -> Code {
    value.code
}

pub fn code(value: &T) -> &'static str {
    value.code.as_str()
}

pub fn message(value: &T) -> &str {
    &value.message
}

pub fn hints(_value: &T) -> &[String] {
    &[]
}

pub fn to_string(value: &T) -> String {
    format!("{} [{}]: {}", value.operation, value.code.as_str(), value.message)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typed_codes_format_and_guard() {
        let error = new("mesh", Code::InvalidTopology, "bad offsets".to_owned());
        assert_eq!(to_string(&error), "mesh [invalid_topology]: bad offsets");
        let guarded = guard(error, "outer", Code::InvalidParameter);
        assert_eq!((operation(&guarded), code(&guarded), message(&guarded)), ("outer", "invalid_parameter", "bad offsets"));
        let stopped = guard(cancelled("inner"), "outer", Code::InvalidParameter);
        assert_eq!((operation(&stopped), stopped.code), ("outer", Code::Cancelled));
        let invalid = make(String::new(), "bad".to_owned(), String::new(), None).unwrap_err();
        assert_eq!(code(&invalid), "invalid_argument");
        assert_eq!(message(&invalid), "Rdk.Error.make: empty operation");
    }
}
