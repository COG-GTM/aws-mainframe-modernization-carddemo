from etl import cli, load


def test_dsn_from_env_uses_conventions_variables():
    env = {"DB_HOST": "aurora.example", "DB_USER": "etl", "DB_PASSWORD": "p w"}
    assert load.dsn_from_env(env) == "host=aurora.example port=5432 dbname=carddemo user=etl password='p w'"
    assert load.dsn_from_env({}) is None


def test_load_without_dsn_or_env_fails_cleanly(monkeypatch, capsys):
    monkeypatch.delenv("DB_HOST", raising=False)
    assert cli.main(["load"]) == 2
    assert "DB_HOST" in capsys.readouterr().err


def test_crosscheck_passes_with_only_known_differences(capsys):
    assert cli.main(["crosscheck"]) == 0
    out = capsys.readouterr().out
    assert "account: 50 rows, 0 unexpected + 1 known" in out
    assert "known: disclosure_group row 34 int_rate" in out
