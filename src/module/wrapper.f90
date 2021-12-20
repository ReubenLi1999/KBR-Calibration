module wrapper
    !> containing all modules

    use Fpl
    use xml_module
    use logger_mod, only: logger_init, logger => master_logger
    use phase_centre_vad
    use math_collection_module
    public

end module wrapper