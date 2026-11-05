// Port of lib/mem/test_mem.ml, assertion for assertion.
use super::*;
use std::sync::atomic::{AtomicI64, AtomicUsize, Ordering};
use std::sync::{LazyLock, Mutex, MutexGuard};

const OCAML_MAX_INT: i64 = (1 << 62) - 1;

static TEST_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));

fn setup() -> MutexGuard<'static, ()> {
    let guard = TEST_LOCK.lock().unwrap_or_else(|error| error.into_inner());
    reset();
    guard
}

fn evictor(
    owner: &Owner,
    rebuild: f64,
    last_use: Option<Arc<dyn Fn() -> i64 + Send + Sync>>,
    live: Option<Arc<dyn Fn() -> bool + Send + Sync>>,
    forget: Option<Arc<dyn Fn() + Send + Sync>>,
    drop_entry: Arc<dyn Fn() -> bool + Send + Sync>,
) {
    register_evictor(
        owner,
        EvictorOptions { rebuild, held: None, last_use, live, forget },
        drop_entry,
    );
}

fn plain_evictor(owner: &Owner, drop_entry: Arc<dyn Fn() -> bool + Send + Sync>) {
    evictor(owner, 1.0, None, None, None, drop_entry);
}

fn constant(value: i64) -> Arc<dyn Fn() -> i64 + Send + Sync> {
    Arc::new(move || value)
}

fn retreat_usage(usage: Arc<dyn Fn() -> i64 + Send + Sync>) -> Verdict {
    retreat_with(RetreatOptions { usage: Some(usage), ..RetreatOptions::default() })
}

fn retreat_device(usage: i64, device: Arc<dyn Fn() -> i64 + Send + Sync>) -> Verdict {
    retreat_with(RetreatOptions { usage: Some(constant(usage)), device: Some(device), ..RetreatOptions::default() })
}

#[test]
fn walked_owners_charge_their_reach() {
    let _guard = setup();
    let named = plan("walk.named");
    let counted = cache("walk.counted");
    let charged = cache("walk.charged");
    let shared = Arc::new(vec![0_i64; 100_000]);
    let held = Arc::new(vec![0_i64; 50_000]);
    let root_shared = shared.clone();
    let root_held = held.clone();
    root(&named, Arc::new(move |visitor| {
        root_shared.visit_allocations(visitor);
        root_held.visit_allocations(visitor);
    }));
    let own = Arc::new(vec![0_i64; 10_000]);
    let counted_table = Arc::new((shared, own));
    walk_table(&counted, counted_table.clone(), Arc::new(|| true));
    walk_table(&charged, held.clone(), Arc::new(|| false));
    charge(&charged, 400_000);
    report();
    assert!((named.bytes() - 800_000).abs() < 100, "a named root is its reach beyond what a table charges");
    assert!((counted.bytes() - 80_000).abs() < 100, "a table is what it reaches beyond the roots before it");
    assert_eq!(charged.bytes(), 400_000, "a table on the ledger is not charged again by a walk");
    root(&named, Arc::new(|_| {}));
    report();
    assert_eq!(named.bytes(), 0, "a dropped root leaves the ledger");
    assert!((counted.bytes() - 880_000).abs() < 100, "what a root let go of falls to the next");
    std::hint::black_box((&counted_table, &held));
}

#[test]
fn charge_release_peak_and_report_fields() {
    let _guard = setup();
    let meshes = cache("renderer.meshes");
    let history = document("editor.history");
    assert!(meshes.bytes() == 0 && meshes.peak() == 0, "fresh owner is empty");
    charge(&meshes, 1_000_000);
    charge(&meshes, 500_000);
    assert!(meshes.bytes() == 1_500_000 && meshes.peak() == 1_500_000, "charge accumulates");
    release(&meshes, 400_000);
    assert!(meshes.bytes() == 1_100_000 && meshes.peak() == 1_500_000, "release keeps peak");
    release(&meshes, 9_000_000);
    assert_eq!(meshes.bytes(), 0, "release floors at zero");
    charge(&history, 12);
    let same = cache("renderer.meshes");
    assert!(same == meshes, "same name reuses the account");
    assert!(meshes.name() == "renderer.meshes" && history.kind() == Kind::Document, "name and kind");
    let report = report_with(ReportOptions { walk: true, device: 64, budget: Some(1_000_000_000) });
    assert_eq!(report.attributed, 12, "report attributed");
    assert_eq!(report.owners.len(), 2, "report lists charged owners");
    assert!(report.device == 64 && report.budget == 1_000_000_000, "report carries device and budget");
    assert!(report.live_bytes > 0 && report.footprint > 0, "live and footprint are positive");
    if let Some(footprint) = phys_footprint() {
        assert!(footprint > 0, "phys_footprint on macOS");
    }
}

#[test]
fn touched_block_raises_the_footprint() {
    let _guard = setup();
    // A touched 100 MB block raises the footprint by about its size (tolerance 10 MB). Windows must report it.
    let before = phys_footprint();
    let block = vec![b'x'; 100_000_000];
    let after = phys_footprint();
    std::hint::black_box(&block);
    match (before, after) {
        (Some(a), Some(b)) => assert!(
            (b - a - 100_000_000).abs() < 10_000_000,
            "footprint rose by {} bytes for a touched 100 MB block (tolerance 10 MB)",
            b - a
        ),
        _ if cfg!(windows) => panic!("phys_footprint is unavailable on Windows"),
        _ => {}
    }
    if let Some(n) = instructions() {
        assert!(n > 0, "instructions");
    }
}

#[test]
fn reset_clears_accounts() {
    let _guard = setup();
    let meshes = cache("renderer.meshes");
    charge(&meshes, 10);
    reset();
    assert!(meshes.bytes() == 0 && report().attributed == 0, "reset clears accounts");
}

#[test]
fn retreat_drops_until_under_budget_then_latches_overdraft() {
    let _guard = setup();
    set_budget(1_000);
    let owner = cache("test.evict");
    let bag = Arc::new(Mutex::new(vec![400_i64, 400, 400]));
    let drain = Arc::clone(&bag);
    let drop_owner = owner.clone();
    plain_evictor(
        &owner,
        Arc::new(move || {
            let mut bag = drain.lock().unwrap();
            match bag.first().copied() {
                None => false,
                Some(n) => {
                    bag.remove(0);
                    release(&drop_owner, n);
                    true
                }
            }
        }),
    );
    charge(&owner, 1_200);
    let usage: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(|| attributed());
    assert!(
        retreat_usage(Arc::clone(&usage)) == Verdict::Within && owner.bytes() == 800 && *bag.lock().unwrap() == vec![400, 400],
        "retreat drops until under budget"
    );
    set_budget(100);
    bag.lock().unwrap().clear();
    assert_eq!(
        retreat_usage(Arc::clone(&usage)),
        Verdict::Over(Overdraft { used: 800, limit: 100, top: vec![("test.evict".to_owned(), 800)] }),
        "retreat is Over, naming the owner, when nothing is left to drop"
    );
    assert!(overdraft().is_some(), "the overdraft is latched");
    set_budget(5_000);
    assert!(retreat_usage(usage) == Verdict::Within && overdraft().is_none(), "recovery clears it");
}

#[test]
fn device_budget_sheds_vram_and_leaves_host_alone() {
    let _guard = setup();
    assert!(!cfg!(windows) || physical_ram() > 0, "Windows reports installed RAM");
    let vram = cache_in(Pool::Device, "test.vram");
    let ram = cache("test.ram");
    let drops = Arc::new(AtomicUsize::new(0));
    let ram_drops = Arc::new(AtomicUsize::new(0));
    let evictable = |owner: &Owner, count: &Arc<AtomicUsize>| {
        let owner = owner.clone();
        let count = Arc::clone(count);
        Arc::new(move || {
            if owner.bytes() > 0 {
                count.fetch_add(1, Ordering::Relaxed);
                release(&owner, 400);
                true
            } else {
                false
            }
        }) as Arc<dyn Fn() -> bool + Send + Sync>
    };
    plain_evictor(&vram, evictable(&vram, &drops));
    plain_evictor(&ram, evictable(&ram, &ram_drops));
    charge(&vram, 1_200);
    charge(&ram, 1_200);
    set_device_budget(1_000);
    let unowned = Arc::new(AtomicI64::new(0));
    let device = {
        let vram = vram.clone();
        let unowned = Arc::clone(&unowned);
        Arc::new(move || vram.bytes() + unowned.load(Ordering::Relaxed)) as Arc<dyn Fn() -> i64 + Send + Sync>
    };
    assert!(
        retreat_device(0, Arc::clone(&device)) == Verdict::Within
            && drops.load(Ordering::Relaxed) == 1
            && ram_drops.load(Ordering::Relaxed) == 0,
        "over the device budget one VRAM entry goes and no host entry"
    );
    set_device_budget(100);
    unowned.store(5_000, Ordering::Relaxed);
    match retreat_device(0, Arc::clone(&device)) {
        Verdict::Over(o) if o.limit == 100 && drops.load(Ordering::Relaxed) == 3 && ram_drops.load(Ordering::Relaxed) == 0 => {}
        other => panic!("device Over names the device limit once every VRAM cache is gone: {other:?}"),
    }
    charge(&vram, 500);
    match admit_bytes(Pool::Device, 1) {
        Err(Refused(o)) if o.limit == 100 => {}
        other => panic!("a device allocation past the device budget is refused: {other:?}"),
    }
    assert!(admit_bytes(Pool::Host, 1).is_ok());
    reset();
}

#[test]
fn retreat_order_is_coldest_and_largest_per_rebuild_cost_then_plans_then_undo() {
    let _guard = setup();
    let order: Arc<Mutex<Vec<String>>> = Arc::default();
    let table = |name: &'static str, size: i64, rebuild: f64, last_use: i64| {
        let owner = cache(name);
        charge(&owner, size);
        let left = Arc::new(AtomicUsize::new(1));
        let order = Arc::clone(&order);
        let drop_owner = owner.clone();
        evictor(
            &owner,
            rebuild,
            Some(Arc::new(move || last_use)),
            None,
            None,
            Arc::new(move || {
                if left.load(Ordering::Relaxed) == 0 {
                    false
                } else {
                    left.fetch_sub(1, Ordering::Relaxed);
                    order.lock().unwrap().push(name.to_owned());
                    release(&drop_owner, size);
                    true
                }
            }),
        );
    };
    for _ in 0..100 {
        retreat();
    }
    let now = epoch();
    table("warm_big", 1000, 1.0, now);
    table("cold_small", 200, 1.0, now - 10);
    table("costly", 1000, 100.0, now);
    let step = |name: &'static str, order: &Arc<Mutex<Vec<String>>>| {
        let order = Arc::clone(order);
        Arc::new(move || {
            let mut order = order.lock().unwrap();
            if order.iter().any(|done| done == name) {
                false
            } else {
                order.push(name.to_owned());
                true
            }
        }) as Arc<dyn Fn() -> bool + Send + Sync>
    };
    set_budget(1);
    let verdict = retreat_with(RetreatOptions {
        usage: Some(constant(2300)),
        later: vec![step("plans", &order), step("undo", &order)],
        ..RetreatOptions::default()
    });
    assert!(matches!(verdict, Verdict::Over(_)), "nothing but evictors freed bytes");
    let got = order.lock().unwrap().join(" ");
    assert_eq!(got, "cold_small warm_big costly plans undo", "retreat order");
}

#[test]
fn busy_cache_is_skipped_once_and_the_next_goes() {
    let _guard = setup();
    set_budget(1);
    let busy = cache("busy");
    let idle = cache("idle");
    charge(&busy, 900);
    charge(&idle, 100);
    let tried = Arc::new(AtomicUsize::new(0));
    let counter = Arc::clone(&tried);
    plain_evictor(
        &busy,
        Arc::new(move || {
            counter.fetch_add(1, Ordering::Relaxed);
            false
        }),
    );
    let idle_owner = idle.clone();
    plain_evictor(
        &idle,
        Arc::new(move || {
            release(&idle_owner, 100);
            true
        }),
    );
    retreat_usage(constant(2));
    assert!(tried.load(Ordering::Relaxed) == 1 && idle.bytes() == 0 && busy.bytes() == 900, "a busy cache is skipped once");
}

#[test]
fn dead_evictor_is_pruned_with_its_bytes() {
    let _guard = setup();
    let gone = cache("gone");
    charge(&gone, 77);
    let forgotten = gone.clone();
    evictor(
        &gone,
        1.0,
        None,
        Some(Arc::new(|| false)),
        Some(Arc::new(move || release(&forgotten, 77))),
        Arc::new(|| true),
    );
    report();
    assert!(evictor_count() == 0 && gone.bytes() == 0, "report prunes dead evictors");
}

#[test]
fn budget_ranks_and_the_process_boundary() {
    let _guard = setup();
    set_budget_from(Source::Machine, 100);
    set_budget_from(Source::Preference, 200);
    set_budget_from(Source::Machine, 300);
    assert_eq!(budget(), 200, "a machine default never replaces a preference");
    let env_4g = |key: &str| (key == "RAYS_MEM_BUDGET").then(|| "4G".to_owned());
    assert_eq!(configure(&env_4g), Ok(()));
    assert!(budget() == 4_000_000_000 && !clobber(), "configure reads a size");
    set_budget_from(Source::Preference, 1);
    assert_eq!(budget(), 4_000_000_000, "the environment beats a preference");
    let env_bad = |key: &str| (key == "RAYS_MEM_BUDGET").then(|| "lots".to_owned());
    assert!(configure(&env_bad).is_err(), "a malformed RAYS_MEM_BUDGET is an error");
    let env_clobber = |key: &str| (key == "RAYS_MEM_CLOBBER").then(|| "1".to_owned());
    assert!(configure(&env_clobber) == Ok(()) && clobber(), "RAYS_MEM_CLOBBER=1 is read");
    let _ = configure(&|_| None);
    match physical_ram() {
        0 => {}
        ram => {
            assert_eq!(machine_budget(0), ram * 6 / 10, "60% of RAM");
            assert_eq!(machine_budget(1000), 1000, "the smaller of the two");
        }
    }
}

#[test]
fn pressure_warning_runs_the_retreat_and_critical_too() {
    let _guard = setup();
    assert!(phys_footprint().is_none() || pressure_start(), "the OS pressure source starts");
    let cached = cache("pressure.cache");
    let big: i64 = 2_000_000_000;
    charge(&cached, big);
    let drop_owner = cached.clone();
    plain_evictor(
        &cached,
        Arc::new(move || {
            if drop_owner.bytes() == 0 {
                false
            } else {
                release(&drop_owner, big);
                true
            }
        }),
    );
    assert_eq!(budget(), 0, "no budget set");
    retreat();
    assert_eq!(cached.bytes(), big, "no pressure, no budget: nothing goes");
    private::pressure_inject(1);
    retreat();
    assert_eq!(cached.bytes(), 0, "warning runs the retreat");
    private::pressure_inject(0);
    retreat();
    charge(&cached, big);
    retreat();
    assert_eq!(cached.bytes(), big, "normal restores the budget");
    private::pressure_inject(2);
    retreat();
    assert_eq!(cached.bytes(), 0, "critical runs the retreat");
    private::pressure_inject(0);
    retreat();
}

#[test]
fn cook_admission_sheds_then_refuses_naming_owners() {
    let _guard = setup();
    let session = cache("cook.session");
    charge(&session, 123);
    assert!(admit().is_ok(), "unlimited admits");
    assert!(admit_bytes(Pool::Host, OCAML_MAX_INT).is_ok());
    let footprint = phys_footprint().unwrap_or(0);
    set_budget_from(Source::Override, footprint + (1 << 30));
    assert!(admit_bytes(Pool::Host, 1 << 20).is_ok());
    match admit_bytes(Pool::Host, 2 << 30) {
        Ok(()) => panic!("an allocation that would pass the limit is refused before it is made"),
        Err(Refused(o)) => assert!(o.used > o.limit, "an allocation that would pass the limit is refused before it is made"),
    }
    set_budget_from(Source::Override, 0);
    set_budget(1);
    let shed_calls = Arc::new(AtomicUsize::new(0));
    let counter = Arc::clone(&shed_calls);
    let mut shed = move || {
        counter.fetch_add(1, Ordering::Relaxed);
        false
    };
    match admit_with(&mut shed) {
        Err(o) if o.limit == 1 && o.top == vec![("cook.session".to_owned(), 123)] && shed_calls.load(Ordering::Relaxed) == 1 => {}
        other => panic!("admit over budget: {other:?}"),
    }
    reset();
}

#[test]
fn drop_whose_bytes_are_still_held_frees_nothing() {
    let _guard = setup();
    let Some(footprint) = phys_footprint() else {
        return;
    };
    let held = cache("held.elsewhere");
    let big: i64 = 1 << 30;
    let dropped = || {
        reset();
        charge(&held, big);
        let owner = held.clone();
        plain_evictor(
            &held,
            Arc::new(move || {
                if owner.bytes() > 0 {
                    release(&owner, big);
                    true
                } else {
                    false
                }
            }),
        );
        set_budget_from(Source::Override, footprint / 2);
        retreat()
    };
    assert_eq!(dropped(), Verdict::Within, "the boundary that drops stands on its credit");
    assert_ne!(retreat(), Verdict::Within, "the next boundary reads what is still there");
    dropped();
    assert!(admit().is_err(), "a cook is not admitted on credit");
    dropped();
    assert!(admit_bytes(Pool::Host, 1 << 20).is_err(), "nor is an allocation");
}

#[test]
fn charges_race_across_threads() {
    let _guard = setup();
    let shared = cache("shared");
    let handles: Vec<_> = (0..4)
        .map(|_| {
            let shared = shared.clone();
            std::thread::spawn(move || {
                for _ in 0..200_000 {
                    charge(&shared, 3);
                    release(&shared, 1);
                }
            })
        })
        .collect();
    for handle in handles {
        handle.join().unwrap();
    }
    assert_eq!(shared.bytes(), 4 * 200_000 * 2, "charges race");
}

#[test]
fn owner_tables_race_across_threads() {
    let _guard = setup();
    let handles: Vec<_> = (0..4)
        .map(|_| {
            std::thread::spawn(move || {
                for i in 1..=2_000 {
                    let owner = cache(&format!("shared.{}", i % 7));
                    charge(&owner, 1);
                    if i % 100 == 0 {
                        plain_evictor(&owner, Arc::new(|| false));
                    }
                    std::hint::black_box(attributed());
                }
            })
        })
        .collect();
    for handle in handles {
        handle.join().unwrap();
    }
    assert_eq!(cache("shared.0").bytes(), 4 * (2_000 / 7), "owner table races");
}
