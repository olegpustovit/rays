open Rays

type t = {
  mutable samples : (string * Audio.Sample.t) list;
  mutable images : (string * Image.t) list;
  mutable audio : Audio.t option;
  mutable closed : bool;
  mutable samples_created : int;
  mutable samples_destroyed : int;
  mutable images_created : int;
  mutable images_destroyed : int;
}

let create () =
  {
    samples = [];
    images = [];
    audio = None;
    closed = false;
    samples_created = 0;
    samples_destroyed = 0;
    images_created = 0;
    images_destroyed = 0;
  }

let error code message = Error (Diagnostic.error ~code message)

let sample resources key make =
  if resources.closed then error "E_AUDIO" "Workspace resources are closed."
  else
    match List.assoc_opt key resources.samples with
    | Some sample -> Ok sample
    | None when List.length resources.samples >= 64 ->
        error "E_AUDIO" "A workspace owns at most 64 audio samples."
    | None ->
        let initialized =
          match resources.audio with
          | Some audio -> Ok audio
          | None ->
              Result.map
                (fun audio ->
                  resources.audio <- Some audio;
                  audio)
                (Audio.init ())
        in
        Result.bind
          (Result.map_error (fun message -> Diagnostic.error ~code:"E_AUDIO" message) initialized)
          (fun audio ->
            Result.map
              (fun sample ->
                resources.samples <- (key, sample) :: resources.samples;
                resources.samples_created <- resources.samples_created + 1;
                sample)
              (Result.map_error
                 (fun message -> Diagnostic.error ~code:"E_AUDIO" message)
                 (make audio)))

let image resources key make =
  if resources.closed then error "E_IMAGE" "Workspace resources are closed."
  else
    match List.assoc_opt key resources.images with
    | Some image -> Ok image
    | None when List.length resources.images >= 64 ->
        error "E_IMAGE" "A workspace owns at most 64 images."
    | None ->
        Result.map
          (fun image ->
            resources.images <- (key, image) :: resources.images;
            resources.images_created <- resources.images_created + 1;
            image)
          (Result.map_error (fun message -> Diagnostic.error ~code:"E_IMAGE" message) (make ()))

let close resources =
  if not resources.closed then begin
    List.iter
      (fun (_, sample) ->
        Audio.Sample.destroy sample;
        resources.samples_destroyed <- resources.samples_destroyed + 1)
      resources.samples;
    List.iter
      (fun (_, image) ->
        ignore (Image.destroy image);
        resources.images_destroyed <- resources.images_destroyed + 1)
      resources.images;
    resources.samples <- [];
    resources.images <- [];
    Option.iter (fun audio -> ignore (Audio.shutdown audio)) resources.audio;
    resources.audio <- None;
    resources.closed <- true
  end
